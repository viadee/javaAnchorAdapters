package smac.impl;

import dataInitialization.DataInitializer;
import evaluationMetrics.PerformanceMeasures;
import evaluationMetrics.PredictionModel;
import configurationSpace.DiscretizerInstantiation.DiscretizerInstantiation;
import randomSearch.Logger;
import smac.util.AnchorsConfig;
import ca.ubc.cs.beta.aeatk.algorithmrunconfiguration.AlgorithmRunConfiguration;
import ca.ubc.cs.beta.aeatk.algorithmrunresult.AlgorithmRunResult;
import ca.ubc.cs.beta.aeatk.algorithmrunresult.ExistingAlgorithmRunResult;
import ca.ubc.cs.beta.aeatk.algorithmrunresult.RunStatus;
import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfiguration;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluator;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluatorCallback;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluatorRunObserver;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.TabularPerturbationFunction;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.UniqueValueDiscretizer;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.execution.sampling.DefaultSamplingFunction;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;

import java.lang.reflect.InvocationTargetException;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.stream.Collectors;

public class CoveragePickTAE implements TargetAlgorithmEvaluator {

    private final Function<TabularInstance, Integer> classificationFunction;
    private final DataInitializer data;
    private final Logger logger;
    private final int[] indexes;
    private final PerformanceMeasures.Measure measure;

    int time = 0;
    private AnchorTabular anchorTabular;

    public CoveragePickTAE(int[] indexes, Function<TabularInstance, Integer> classificationFunction, DataInitializer data, PerformanceMeasures.Measure measure, Logger logger) {
        this.indexes = indexes;
        this.classificationFunction = classificationFunction;
        this.measure = measure;
        this.data = data;
        this.logger = logger;
        this.anchorTabular = data.createTabular(null);
    }

    private PerformanceMeasures calcPerformance(List<AnchorResult<TabularInstance>> explanations, AnchorTabular anchorTabular, Function<TabularInstance, Integer> model) {

        // calculate performance as a minimization problem
        PredictionModel predictionModel = new PredictionModel(explanations);
        List<Integer> prediction = predictionModel.predict(anchorTabular.getTabularInstances());
        PerformanceMeasures performance = new PerformanceMeasures(prediction, model, anchorTabular.getTabularInstances());
        return performance;

    }

    private void setDiscretizers(ParameterConfiguration configuration) {

        final Map<String, String> parameters = configuration.getActiveParameters().stream()
                .map(p -> new HashMap.SimpleImmutableEntry<>(p, configuration.get(p)))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        Map<String, Discretizer> nameToDiscretizer = new HashMap<>();

        for (GenericColumn column : anchorTabular.getColumns()) {

            if (!column.getDiscretizer().getClass().getSimpleName().equals(UniqueValueDiscretizer.class.getSimpleName())) {
                Map<String, String> columnDiscretizerParameters = parameters.entrySet().stream()
                        .sorted(Map.Entry.comparingByKey())
                        .filter(e -> e.getKey().toLowerCase().contains(column.getName().toLowerCase()))
                        .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                                (oldValue, newValue) -> oldValue, LinkedHashMap::new));

                List<Map.Entry<String, String>> test = columnDiscretizerParameters.entrySet().stream()
                        .collect(Collectors.toList());

                try {
                    final String constructorClassName = "configurationSpace.DiscretizerInstantiation." +
                            test.get(0).getValue() + "Instantiation";

                    final DiscretizerInstantiation discretizerConstructor = (DiscretizerInstantiation) Class.forName(constructorClassName)
                            .getConstructor().newInstance();

                    Discretizer newDiscretizer = discretizerConstructor.constructDiscretizer(test.subList(1, test.size()));

                    nameToDiscretizer.put(column.getName(), newDiscretizer);
                } catch (ClassNotFoundException | NoSuchMethodException | InstantiationException | IllegalAccessException | InvocationTargetException e) {
                    throw new RuntimeException(e);
                }
            }
        }

        anchorTabular = data.createTabular(nameToDiscretizer);
    }

    @Override
    public List<AlgorithmRunResult> evaluateRun(AlgorithmRunConfiguration algorithmRunConfiguration) {
        return this.evaluateRun(Arrays.asList(algorithmRunConfiguration), null);
    }

    @Override
    public List<AlgorithmRunResult> evaluateRun(List<AlgorithmRunConfiguration> list) {
        return this.evaluateRun(list, null);
    }

    @Override
    public List<AlgorithmRunResult> evaluateRun(List<AlgorithmRunConfiguration> list, TargetAlgorithmEvaluatorRunObserver targetAlgorithmEvaluatorRunObserver) {
        List<AlgorithmRunResult> ar = new ArrayList<>();

        for (AlgorithmRunConfiguration ac : list) {

            ParameterConfiguration configuration = ac.getParameterConfiguration();

            long runtimeStart = System.currentTimeMillis();

            setDiscretizers(configuration);

            final TabularInstance explainedInstance = anchorTabular.getTabularInstances()[0];
            final TabularPerturbationFunction perturbationFunction = new TabularPerturbationFunction(explainedInstance, anchorTabular.getTabularInstances());
            final Integer explainedInstanceLabel = classificationFunction.apply(explainedInstance);

            AnchorConstructionBuilder anchorBuilder = setAnchorConstructionBuilder(perturbationFunction, explainedInstanceLabel, explainedInstance);

            TabularInstance[] instances = new TabularInstance[indexes.length];
            for (int i = 0; i < indexes.length; i++) {
                instances[i] = anchorTabular.getTabularInstances()[indexes[i]];
            }

            List<AnchorResult<TabularInstance>> rules = new CoveragePick<>(AnchorsConfig.setParameters(configuration, anchorBuilder), 10,
                    Executors.newCachedThreadPool(), null)
                    .run(instances, 20);

            PerformanceMeasures measures = calcPerformance(rules, anchorTabular, classificationFunction);

            // evaluation
            final double runtime = System.currentTimeMillis() - runtimeStart;
            final double coverage = measures.getCoverage();
            final double performance = measures.calcMeasure(measure);
            final double quality = 1 - (coverage * performance);

            // log results
            logger.addSMACValues(configuration);
            logger.addValuesToLogging(runtime, coverage, performance);
            for (AnchorResult rule : rules) {
                logger.addRulesToLogging(anchorTabular.getVisualizer().visualizeResult(rule));
            }
            logger.endLine();

            ar.add(new ExistingAlgorithmRunResult(ac, RunStatus.SAT, runtime, 1, quality , 10L));
        }
        return ar;
    }

    /**
     * Create the AnchorsConstructionBuilder with the perturbation function, the label of the explained instance and the explained instance self. Set the emptyRuleEvaluation to 0 to exclude the empty
     * rule from being selected as anchor.
     *
     * @param perturbationFunction   the perturbation function
     * @param explainedInstanceLabel the label of the explained instance
     * @param explainedInstance      the explained instance
     * @return the AnchorConstructionBuilder
     */
    private AnchorConstructionBuilder setAnchorConstructionBuilder(TabularPerturbationFunction perturbationFunction, final Integer explainedInstanceLabel, TabularInstance explainedInstance) {
        return new AnchorConstructionBuilder<>(
                new DefaultSamplingFunction<>(classificationFunction::apply, perturbationFunction),
                explainedInstance, explainedInstanceLabel)
                .setEmptyRuleEvaluations(0);
    }


    @Override
    public void evaluateRunsAsync(AlgorithmRunConfiguration algorithmRunConfiguration, TargetAlgorithmEvaluatorCallback targetAlgorithmEvaluatorCallback) {

    }

    @Override
    public void evaluateRunsAsync(List<AlgorithmRunConfiguration> list, TargetAlgorithmEvaluatorCallback targetAlgorithmEvaluatorCallback) {

    }

    @Override
    public void evaluateRunsAsync(List<AlgorithmRunConfiguration> list, TargetAlgorithmEvaluatorCallback targetAlgorithmEvaluatorCallback, TargetAlgorithmEvaluatorRunObserver targetAlgorithmEvaluatorRunObserver) {

    }

    @Override
    public void waitForOutstandingEvaluations() {

    }

    @Override
    public int getNumberOfOutstandingEvaluations() {
        return 0;
    }

    @Override
    public int getNumberOfOutstandingBatches() {
        return 0;
    }

    @Override
    public int getNumberOfOutstandingRuns() {
        return 0;
    }

    @Override
    public int getRunCount() {
        return 0;
    }

    @Override
    public int getRunHash() {
        return 0;
    }

    @Override
    public void seek(List<AlgorithmRunResult> list) {

    }

    @Override
    public String getManualCallString(AlgorithmRunConfiguration algorithmRunConfiguration) {
        return null;
    }

    @Override
    public void notifyShutdown() {

    }

    @Override
    public boolean isRunFinal() {
        return false;
    }

    @Override
    public boolean areRunsPersisted() {
        return false;
    }

    @Override
    public boolean areRunsObservable() {
        return false;
    }

    @Override
    public void close() {

    }
}
