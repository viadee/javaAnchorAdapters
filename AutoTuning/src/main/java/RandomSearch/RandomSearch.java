package RandomSearch;

import LossFunctions.PredictionModel;
import LossFunctions.PerformanceMeasures;
import Parameter.NumericalParameter;
import Parameter.Parameter;
import Parameter.CategoricalParameter;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;
import de.viadee.xai.anchor.algorithm.util.ParameterValidation;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.function.Function;

/**
 * Class for optimizing a given configuration space for Anchor construction
 */
public class RandomSearch {

    private final String scenario;
    private final AnchorConstructionBuilder<TabularInstance> anchorBuilder;
    private final AnchorTabular anchorTabular;
    private final Function<TabularInstance, Integer> classificationFunction;
    private final long terminationConditionInSec;
    private final int terminationConditionNrEx;
    private final PerformanceMeasures.Measure measure;
    private ConfigurationSpace currentConfigurationSpace;
    private ConfigurationSpace bestConfigurationSpace;
    private List<AnchorResult<TabularInstance>> bestExplanations;

    RandomSearch(String scenario,
                 AnchorConstructionBuilder<TabularInstance> anchorBuilder,
                 AnchorTabular anchorTabular,
                 ConfigurationSpace configurationSpace,
                 long terminationConditionInSec,
                 int terminationConditionNrEx,
                 boolean startWithDefault,
                 Function<TabularInstance, Integer> classificationFunction,
                 PerformanceMeasures.Measure measure) {

        if (configurationSpace == null)
            throw new IllegalArgumentException("Configuration space " + ParameterValidation.NULL_MESSAGE);
        if (anchorBuilder == null)
            throw new IllegalArgumentException("AnchorConstructionBuilder " + ParameterValidation.NULL_MESSAGE);
        if (anchorTabular == null)
            throw new IllegalArgumentException("AnchorTabular " + ParameterValidation.NULL_MESSAGE);
        if (classificationFunction == null)
            throw new IllegalArgumentException("Classification function " + ParameterValidation.NULL_MESSAGE);
        if (terminationConditionInSec == 0 && terminationConditionNrEx == 0)
            throw new IllegalArgumentException("No termination condition defined to run random search");

        this.scenario = scenario;
        this.anchorBuilder = anchorBuilder;
        this.anchorTabular = anchorTabular;
        this.currentConfigurationSpace = configurationSpace;
        this.classificationFunction = classificationFunction;
        this.measure = measure;
        this.terminationConditionInSec = terminationConditionInSec;
        this.terminationConditionNrEx = terminationConditionNrEx;

        this.bestConfigurationSpace = new ConfigurationSpace(configurationSpace);

        if (!startWithDefault)
            currentConfigurationSpace.getHyperparameterSpace().randomizeParameters();

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
            setNewParameters();

            List<AnchorResult<TabularInstance>> rules = global ? optimizeGlobal() : optimizeLocal();

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
            //logger.addRulesToLogging(anchorTabular.getVisualizer().visualizeGlobalResults(rules));
            logger.endLine();

            // check if performance of current space is the best, if yes set current space as best space
            if (checkIfBetter(currentConfigurationSpace.getPerformance() * currentConfigurationSpace.getCoverage())) {
                bestConfigurationSpace = new ConfigurationSpace(currentConfigurationSpace);
                bestExplanations = rules;
            }

            // randomize all hyperparameters
            currentConfigurationSpace.getHyperparameterSpace().randomizeParameters();
            currentConfigurationSpace.getDiscretizationSpace().randomizeParameters();
            nrExecutions++;
        }

        //randomSearchLogger.addRulesToLogging(anchorTabular.getVisualizer().visualizeGlobalResults(bestExplanations));
        logger.endLogging();

        // visualize best hyperparameters and the best global explanations
        visualizeBestHyperparameterSpace(measure);
        System.out.println(anchorTabular.getVisualizer().visualizeGlobalResults(bestExplanations));
    }

    private List<AnchorResult<TabularInstance>> optimizeGlobal() {
        // execute Coverage Pick of Anchors and get result
        return new CoveragePick<>(anchorBuilder, 10,
                Executors.newCachedThreadPool(), null)
                .run(anchorTabular.getTabularInstances(), 20);
    }

    private List<AnchorResult<TabularInstance>> optimizeLocal() {
        final AnchorResult<TabularInstance> localExplanation = anchorBuilder.build().constructAnchor();
        return Arrays.asList(localExplanation);
    }

    private void setNewDiscretizers() {

        DiscretizationSpace ds = currentConfigurationSpace.getDiscretizationSpace();

        if (ds != null){
            for (int i = 0; i < ds.getDiscretizerParamter().size(); i++) {
                CategoricalParameter p = ds.getDiscretizerParamter().get(i);
                if (anchorTabular.getColumns().get(i).getDiscretizer().getClass() != UniqueValueDiscretizer.class)
                    anchorTabular.getColumns().get(i).setDiscretizer((Discretizer) p.getCurrentValue());
            }
        }
    }

    private void setNewParameters() {

        HyperparameterSpace hs = currentConfigurationSpace.getHyperparameterSpace();

        if (hs.getParameterByName("tau") != null)
            anchorBuilder.setTau(((NumericalParameter) hs.getParameterByName("tau")).getCurrentValue().intValue());
        if (hs.getParameterByName("beamsize") != null)
            anchorBuilder.setBeamSize(((NumericalParameter) hs.getParameterByName("beamsize")).getCurrentValue().intValue());
        if (hs.getParameterByName("delta") != null)
            anchorBuilder.setDelta(((NumericalParameter) hs.getParameterByName("delta")).getCurrentValue().doubleValue());
        if (hs.getParameterByName("epsilon") != null)
            anchorBuilder.setEpsilon(((NumericalParameter) hs.getParameterByName("epsilon")).getCurrentValue().doubleValue());
        if (hs.getParameterByName("tauDiscrepancy") != null)
            anchorBuilder.setTauDiscrepancy(((NumericalParameter) hs.getParameterByName("tauDiscrepancy")).getCurrentValue().doubleValue());
        if (hs.getParameterByName("initSampleCount") != null)
            anchorBuilder.setInitSampleCount(((NumericalParameter) hs.getParameterByName("initSampleCount")).getCurrentValue().intValue());
    }

    /**
     * @param performance
     */
    private boolean checkIfBetter(double performance) {
        return performance > this.bestConfigurationSpace.getPerformance() * this.bestConfigurationSpace.getCoverage() ? true : false;
    }


    private void visualizeBestHyperparameterSpace(PerformanceMeasures.Measure measure) {

        StringBuilder sb = new StringBuilder();
        for (Parameter p : this.bestConfigurationSpace.getHyperparameterSpace().getHyperParameters()) {
            sb.append(p.getName() + ": " + p.getCurrentValue() + System.lineSeparator());
        }

        System.out.println("==== The best Hyperparameter Space is ====" + System.lineSeparator() +
                sb.toString() +
                "coverage: " + this.bestConfigurationSpace.getCoverage() + System.lineSeparator() +
                measure.toString().toLowerCase() + ": " + this.bestConfigurationSpace.getPerformance() + System.lineSeparator() +
                "runtime: " + this.bestConfigurationSpace.getRuntime() + "ms");
    }
}
