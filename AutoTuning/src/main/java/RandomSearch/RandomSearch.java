package RandomSearch;

import LossFunctions.PredictionModel;
import LossFunctions.PerformanceMeasures;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.function.Function;


public class RandomSearch {

    private final String scenario;
    private final AnchorConstructionBuilder<TabularInstance> anchorBuilder;
    private final AnchorTabular anchorTabular;
    private final long terminationConditionInSec;
    private final int terminationConditionNrEx;
    private HyperparameterSpace currentHyperparameterSpace;
    private HyperparameterSpace bestHyperparameterSpace;
    private List<AnchorResult<TabularInstance>> bestExplanations;
    private RandomSearchLogger logger;

    private RandomSearch(String scenario, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular, long terminationConditionInSec, int terminationConditionNrEx, boolean startWithDefault) {
        this.scenario = scenario;
        this.anchorBuilder = anchorBuilder;
        this.anchorTabular = anchorTabular;
        this.bestHyperparameterSpace = new HyperparameterSpace();
        this.currentHyperparameterSpace = new HyperparameterSpace();
        if (!startWithDefault) {
            this.currentHyperparameterSpace = randomizeHyperparameters(currentHyperparameterSpace);
        }
        this.terminationConditionInSec = terminationConditionInSec;
        this.terminationConditionNrEx = terminationConditionNrEx;
    }

    public RandomSearch(String scenario, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular, long terminationConditionInSec, boolean startWithDefault) {
        this(scenario, anchorBuilder, anchorTabular, terminationConditionInSec, 0, startWithDefault);
    }

    public RandomSearch(String scenario, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular, int terminationConditionNrEx, boolean startWithDefault) {
        this(scenario, anchorBuilder, anchorTabular, 0, terminationConditionNrEx, startWithDefault);
    }

    public RandomSearch(String scenario, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular, long terminationConditionInSec) {
        this(scenario, anchorBuilder, anchorTabular, terminationConditionInSec, 0, true);
    }

    public RandomSearch(String scenario, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular, int terminationConditionNrEx) {
        this(scenario, anchorBuilder, anchorTabular, 0, terminationConditionNrEx, true);
    }

    public void optimizeExplanations(Function<TabularInstance, Integer> classificationFunction, AnchorTabular anchorTabular, PerformanceMeasures.Measure measure, boolean global) {

        long startTime = System.currentTimeMillis();
        int nrExecutions = 0;

        this.logger = new RandomSearchLogger(scenario, bestHyperparameterSpace, measure);

        while ((System.currentTimeMillis() - startTime) < (terminationConditionInSec * 1000) || nrExecutions < this.terminationConditionNrEx) {

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            // set all hyperparameters
            setNewParameters();

            List<AnchorResult<TabularInstance>> rules = global ? optimizeGlobal() : optimizeLocal();

            // set runtime of current Anchors run
            currentHyperparameterSpace.setRuntime(System.currentTimeMillis() - runtimeStart);

            // predict labels of instances based on generated global rules
            PredictionModel model = new PredictionModel(rules);
            List<Integer> prediction = model.predict(anchorTabular.getTabularInstances());
            PerformanceMeasures performanceMeasures = new PerformanceMeasures(prediction, classificationFunction, anchorTabular.getTabularInstances());
            currentHyperparameterSpace.setPerformance(performanceMeasures.calcMeasure(measure));
            currentHyperparameterSpace.setCoverage(performanceMeasures.getCoverage());

            // log results
            logger.addValuesToLogging(currentHyperparameterSpace);
            logger.addRulesToLogging(anchorTabular.getVisualizer().visualizeGlobalResults(rules));
            logger.endLine();

            // check if performance of current space is the best, if yes set current space as best space
            if (checkIfBetter(currentHyperparameterSpace.getPerformance() * currentHyperparameterSpace.getCoverage())) {
                bestHyperparameterSpace = new HyperparameterSpace(currentHyperparameterSpace);
                bestExplanations = rules;
            }

            // randomize all hyperparameters
            currentHyperparameterSpace = randomizeHyperparameters(currentHyperparameterSpace);

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

    private void setNewParameters() {

        anchorBuilder
                .setTau(((NumericalParameter)currentHyperparameterSpace.getParameterByName("tau")).getCurrentValue().doubleValue())
                .setBeamSize(((NumericalParameter)currentHyperparameterSpace.getParameterByName("beamsize")).getCurrentValue().intValue())
                .setDelta(((NumericalParameter)currentHyperparameterSpace.getParameterByName("delta")).getCurrentValue().doubleValue())
                .setEpsilon(((NumericalParameter)currentHyperparameterSpace.getParameterByName("epsilon")).getCurrentValue().doubleValue())
                .setTauDiscrepancy(((NumericalParameter)currentHyperparameterSpace.getParameterByName("tauDiscrepancy")).getCurrentValue().doubleValue())
                .setInitSampleCount(((NumericalParameter)currentHyperparameterSpace.getParameterByName("initSampleCount")).getCurrentValue().intValue());
    }


    private HyperparameterSpace randomizeHyperparameters(HyperparameterSpace hyperparameterSpace) {

        List<Parameter> randomParameters = new ArrayList<Parameter>();

        for (Parameter p : hyperparameterSpace.getHyperParameters()) {
            p.searchRandom();
            randomParameters.add(p);
        }

        return hyperparameterSpace;
    }

    /**
     * @param performance
     */
    private boolean checkIfBetter(double performance) {
        return performance > this.bestHyperparameterSpace.getPerformance() * this.bestHyperparameterSpace.getCoverage() ? true : false;
    }


    private void visualizeBestHyperparameterSpace(PerformanceMeasures.Measure measure) {

        StringBuilder sb = new StringBuilder();
        for (Parameter p : this.bestHyperparameterSpace.getHyperParameters()) {
            sb.append(p.getName() + ": " + p.getCurrentValue() + System.lineSeparator());
        }

        System.out.println("==== The best Hyperparameter Space is ====" + System.lineSeparator() +
                sb.toString() +
                "coverage: " + this.bestHyperparameterSpace.getCoverage() + System.lineSeparator() +
                measure.toString().toLowerCase() + ": " + this.bestHyperparameterSpace.getPerformance() + System.lineSeparator() +
                "runtime: " + this.bestHyperparameterSpace.getRuntime() + "ms");
    }

    public HyperparameterSpace getCurrentHyperparameterSpace() {
        return currentHyperparameterSpace;
    }
}
