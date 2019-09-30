package smac.impl;

import dataInitialization.DataInitializer;
import evaluationMetrics.PerformanceMeasures;
import evaluationMetrics.TabularPredictionModel;
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
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.execution.sampling.DefaultSamplingFunction;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;

import java.util.*;
import java.util.concurrent.Executors;
import java.util.function.Function;

/**
 * The target algorithm evaluator executing the Coverage Pick algorithm with a selected configuration from SMAC.
 * The evaluation's value is processed to SMAC
 */
public class CoveragePickTAE implements TargetAlgorithmEvaluator {

    private final Function<TabularInstance, Integer> classificationFunction;
    private final DataInitializer data;
    private final Logger logger;
    private final int[] indexes;
    private final PerformanceMeasures.Measure measure;

    int time = 0;
    private AnchorTabular anchorTabular;

    public CoveragePickTAE(Function<TabularInstance, Integer> classificationFunction, DataInitializer data, Logger logger, PerformanceMeasures.Measure measure) {
        this(null,classificationFunction,data,measure,logger);
    }

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
        TabularPredictionModel tabularPredictionModel = new TabularPredictionModel(explanations);
        List<Integer> prediction = tabularPredictionModel.predict(anchorTabular.getTabularInstances());
        return new PerformanceMeasures(prediction, model, anchorTabular.getTabularInstances());

    }

    @Override
    public List<AlgorithmRunResult> evaluateRun(AlgorithmRunConfiguration algorithmRunConfiguration) {
        return this.evaluateRun(Collections.singletonList(algorithmRunConfiguration), null);
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

            anchorTabular = data.createTabular(AnchorsConfig.setDiscretizerForSmac(configuration, anchorTabular));

            final TabularInstance explainedInstance = anchorTabular.getTabularInstances()[0];
            final TabularPerturbationFunction perturbationFunction = new TabularPerturbationFunction(explainedInstance, anchorTabular.getTabularInstances());
            final Integer explainedInstanceLabel = classificationFunction.apply(explainedInstance);

            AnchorConstructionBuilder anchorBuilder = setAnchorConstructionBuilder(perturbationFunction, explainedInstanceLabel, explainedInstance);

            TabularInstance[] instances;
            if (indexes == null) {
                instances = anchorTabular.getTabularInstances();
            } else {
                instances = new TabularInstance[indexes.length];
                for (int i = 0; i < indexes.length; i++) {
                    instances[i] = anchorTabular.getTabularInstances()[indexes[i]];
                }
            }

            List<AnchorResult<TabularInstance>> rules = new CoveragePick<>(AnchorsConfig.setParametersForSmac(configuration, anchorBuilder), 10,
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

            ar.add(new ExistingAlgorithmRunResult(ac, RunStatus.SAT, runtime, 1, quality , 10L, anchorTabular.getVisualizer().visualizeGlobalResults(rules)));
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
