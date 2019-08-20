package RandomSearch;

import LossFunctions.PerformanceMeasures;
import LossFunctions.PredictionModel;
import DataInitialization.DataInitializer;
import Parameter.NumericalParameter;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.UniqueValueDiscretizer;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;
import de.viadee.xai.anchor.algorithm.util.ParameterValidation;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.function.Function;

/**
 * Class for optimizing a given configuration space for Anchor construction
 */
public class RandomSearch {

    private final String scenario;
    private final int explainedInstanceIndex;
    private final Function<TabularInstance, Integer> classificationFunction;
    private final long terminationConditionInSec;
    private final int terminationConditionNrEx;
    private final PerformanceMeasures.Measure measure;
    private final DataInitializer data;
    private AnchorTabular anchorTabular;
    private ConfigurationSpace currentConfigurationSpace;
    private ConfigurationSpace bestConfigurationSpace;
    private List<AnchorResult<TabularInstance>> bestExplanations;

    RandomSearch(String scenario,
                 int explainedInstanceIndex,
                 DataInitializer data,
                 ConfigurationSpace configurationSpace,
                 long terminationConditionInSec,
                 int terminationConditionNrEx,
                 boolean startWithDefault,
                 Function<TabularInstance, Integer> classificationFunction,
                 PerformanceMeasures.Measure measure) {

        if (configurationSpace == null)
            throw new IllegalArgumentException("Configuration space " + ParameterValidation.NULL_MESSAGE);
        if (explainedInstanceIndex == 0)
            throw new IllegalArgumentException("explainedInstanceIndex " + ParameterValidation.NULL_MESSAGE);
        if (data == null)
            throw new IllegalArgumentException("Data " + ParameterValidation.NULL_MESSAGE);
        if (classificationFunction == null)
            throw new IllegalArgumentException("Classification function " + ParameterValidation.NULL_MESSAGE);
        if (terminationConditionInSec == 0 && terminationConditionNrEx == 0)
            throw new IllegalArgumentException("No termination condition defined to run random search");

        this.scenario = scenario;
        this.explainedInstanceIndex = explainedInstanceIndex;
        this.data = data;
        this.currentConfigurationSpace = configurationSpace;
        this.classificationFunction = classificationFunction;
        this.measure = measure;
        this.terminationConditionInSec = terminationConditionInSec;
        this.terminationConditionNrEx = terminationConditionNrEx;

        this.bestConfigurationSpace = new ConfigurationSpace(configurationSpace);
        this.anchorTabular = data.createTabular(null);
    }

    /**
     * Optimize the explanations given by Anchors either for a certain local instance or for the global explanations created
     * with the coverage pick. The output is a set of optimal parameters, their performance and the resulting rule/s
     *
     * @param global should the optimization be on a global level
     */
    public void optimizeExplanations(boolean global) {

        long startTime = System.currentTimeMillis();
        int nrExecutions = 0;

        RandomSearchLogger logger = new RandomSearchLogger(scenario, currentConfigurationSpace, measure);

        while ((System.currentTimeMillis() - startTime) < (terminationConditionInSec * 1000) || nrExecutions < this.terminationConditionNrEx) {

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            setNewDiscretizers();

            TabularInstance explainedInstance = anchorTabular.getTabularInstances()[explainedInstanceIndex];
            final AnchorConstructionBuilder<TabularInstance> anchorBuilder = anchorTabular
                    .createDefaultBuilder(classificationFunction, explainedInstance);

            setNewParameters(anchorBuilder);

            List<AnchorResult<TabularInstance>> rules = global ? optimizeGlobal(anchorBuilder) : optimizeLocal(anchorBuilder);

            // set runtime of current Anchors run
            currentConfigurationSpace.setRuntime(System.currentTimeMillis() - runtimeStart);

            // predict labels of instances based on generated global rules
            PredictionModel model = new PredictionModel(rules);
            List<Integer> prediction = model.predict(anchorTabular.getTabularInstances());
            PerformanceMeasures performanceMeasures = new PerformanceMeasures(prediction, classificationFunction, anchorTabular.getTabularInstances());
            currentConfigurationSpace.setPerformance(performanceMeasures.calcMeasure(measure));
            currentConfigurationSpace.setCoverage(performanceMeasures.getCoverage());

            // log results
            logger.addValuesToLogging(currentConfigurationSpace);
            logger.addRulesToLogging(anchorTabular.getVisualizer().visualizeGlobalResults(rules));
            logger.endLine();

            // check if performance of current space is the best, if yes set current space as best space
            if (checkIfBetter(currentConfigurationSpace.getPerformance() * currentConfigurationSpace.getCoverage())) {
                bestConfigurationSpace = new ConfigurationSpace(currentConfigurationSpace);
                bestExplanations = rules;
            }
            nrExecutions++;
        }

        //randomSearchLogger.addRulesToLogging(anchorTabular.getVisualizer().visualizeGlobalResults(bestExplanations));
        logger.endLogging();

        // visualize best hyperparameters and the best global explanations
        visualizeBestHyperparameterSpace(measure);
        System.out.println(anchorTabular.getVisualizer().visualizeGlobalResults(bestExplanations));
    }

    private List<AnchorResult<TabularInstance>> optimizeGlobal(AnchorConstructionBuilder<TabularInstance> anchorBuilder) {
        return new CoveragePick<>(anchorBuilder, 10,
                Executors.newCachedThreadPool(), null)
                .run(anchorTabular.getTabularInstances(), 20);
    }

    private List<AnchorResult<TabularInstance>> optimizeLocal(AnchorConstructionBuilder<TabularInstance> anchorBuilder) {
        final AnchorResult<TabularInstance> localExplanation = anchorBuilder.build().constructAnchor();
        return Arrays.asList(localExplanation);
    }

    private void setNewDiscretizers() {

        DiscretizationSpace ds = currentConfigurationSpace.getDiscretizationSpace();
        Map<String, Discretizer> discretizerMap = new HashMap<>();

        if (ds != null) {
            for (GenericColumn column : anchorTabular.getColumns()) {
                if (column.getDiscretizer().getClass() != UniqueValueDiscretizer.class)
                    discretizerMap.put(column.getName(), ds.getRandomDiscretizer());
            }
        }

        this.anchorTabular = data.createTabular(discretizerMap);
    }

    private void setNewParameters(AnchorConstructionBuilder<TabularInstance> anchorBuilder) {

        HyperparameterSpace hs = currentConfigurationSpace.getHyperparameterSpace();

        if (hs.getParameterByName("tau") != null)
            anchorBuilder.setTau(((NumericalParameter) hs.getParameterByName("tau")).getRandomValue().intValue());
        if (hs.getParameterByName("beamsize") != null)
            anchorBuilder.setBeamSize(((NumericalParameter) hs.getParameterByName("beamsize")).getRandomValue().intValue());
        if (hs.getParameterByName("delta") != null)
            anchorBuilder.setDelta(((NumericalParameter) hs.getParameterByName("delta")).getRandomValue().doubleValue());
        if (hs.getParameterByName("epsilon") != null)
            anchorBuilder.setEpsilon(((NumericalParameter) hs.getParameterByName("epsilon")).getRandomValue().doubleValue());
        if (hs.getParameterByName("tauDiscrepancy") != null)
            anchorBuilder.setTauDiscrepancy(((NumericalParameter) hs.getParameterByName("tauDiscrepancy")).getRandomValue().doubleValue());
        if (hs.getParameterByName("initSampleCount") != null)
            anchorBuilder.setInitSampleCount(((NumericalParameter) hs.getParameterByName("initSampleCount")).getRandomValue().intValue());
    }

    private boolean checkIfBetter(double performance) {
        return performance > this.bestConfigurationSpace.getPerformance() * this.bestConfigurationSpace.getCoverage() ? true : false;
    }

    private void visualizeBestHyperparameterSpace(PerformanceMeasures.Measure measure) {
        System.out.println("==== incumbent performance ====" + System.lineSeparator() +
                "coverage: " + this.bestConfigurationSpace.getCoverage() + System.lineSeparator() +
                measure.toString().toLowerCase() + ": " + this.bestConfigurationSpace.getPerformance() + System.lineSeparator() +
                "runtime: " + this.bestConfigurationSpace.getRuntime() + "ms");
    }
}
