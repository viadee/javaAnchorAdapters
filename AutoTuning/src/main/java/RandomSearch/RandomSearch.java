package RandomSearch;

import LossFunctions.Accuracy.PredictionModel;
import LossFunctions.PerformanceMeasures;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.function.Function;


public class RandomSearch {

    private final String scenario;
    private final long terminationConditionInSec;
    private int terminationConditionNrEx;
    private HyperparameterSpace currentHyperparameterSpace;
    private HyperparameterSpace bestHyperparameterSpace;
    private List<AnchorResult<TabularInstance>> bestGlobalExplanations;

    /**
     * @param terminationConditionInSec
     */
    private RandomSearch(String scenario, long terminationConditionInSec, int terminationConditionNrEx, boolean startWithDefault) {
        this.scenario = scenario;
        this.bestHyperparameterSpace = new HyperparameterSpace();
        this.currentHyperparameterSpace = new HyperparameterSpace();
        if (!startWithDefault) {
            this.currentHyperparameterSpace = randomizeHyperparameters(currentHyperparameterSpace);
        }
        this.terminationConditionInSec = terminationConditionInSec;
        this.terminationConditionNrEx = terminationConditionNrEx;


    }

    public RandomSearch(String scenario, long terminationConditionInSec, boolean startWithDefault) {
        this(scenario, terminationConditionInSec, 0, startWithDefault);
    }

    public RandomSearch(String scenario, int terminationConditionNrEx, boolean startWithDefault) {
        this(scenario, 0, terminationConditionNrEx, startWithDefault);
    }

    public RandomSearch(String scenario, long terminationConditionInSec) {
        this(scenario, terminationConditionInSec, 0, true);
    }

    public RandomSearch(String scenario, int terminationConditionNrEx) {
        this(scenario, 0, terminationConditionNrEx, true);
    }

    /**
     * @param classificationFunction
     * @param anchorBuilder
     * @param anchorTabular
     */
    public void execute(Function<TabularInstance, Integer> classificationFunction, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular, PerformanceMeasures.Measure measure) {

        long startTime = System.currentTimeMillis();
        int nrExecutions = 0;

        // init logging the configurations
        RandomSearchLogger randomSearchLogger = new RandomSearchLogger(scenario, bestHyperparameterSpace);

        while ((System.currentTimeMillis() - startTime) < (terminationConditionInSec * 1000) || nrExecutions < this.terminationConditionNrEx) {

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            // set all hyperparameters

            anchorBuilder
                    .setTau(currentHyperparameterSpace.getParameterByName("tau").getCurrentValue().doubleValue())
                    .setBeamSize(currentHyperparameterSpace.getParameterByName("beamsize").getCurrentValue().intValue())
                    .setDelta(currentHyperparameterSpace.getParameterByName("delta").getCurrentValue().doubleValue())
                    .setEpsilon(currentHyperparameterSpace.getParameterByName("epsilon").getCurrentValue().doubleValue())
                    .setTauDiscrepancy(currentHyperparameterSpace.getParameterByName("tauDiscrepancy").getCurrentValue().doubleValue())
                    .setInitSampleCount(currentHyperparameterSpace.getParameterByName("initSampleCount").getCurrentValue().intValue());


            // execute Coverage Pick of Anchors and get result
            final List<AnchorResult<TabularInstance>> globalExplanations = new CoveragePick<>(anchorBuilder, 10,
                    Executors.newCachedThreadPool(), null)
                    .run(anchorTabular.getTabularInstances(), 20);

            // set runtime of current Anchors run
            currentHyperparameterSpace.setRuntime(System.currentTimeMillis() - runtimeStart);

            // predict labels of instances based on generated global rules
            PredictionModel model = new PredictionModel(globalExplanations);
            List<Integer> prediction = model.predict(anchorTabular.getTabularInstances());

            // init performance measure
            PerformanceMeasures performanceMeasures = new PerformanceMeasures(prediction, classificationFunction, anchorTabular.getTabularInstances());
            currentHyperparameterSpace.setPerformance(performanceMeasures.calcMeasure(measure));
            currentHyperparameterSpace.setCoverage(performanceMeasures.getCoverage());
            randomSearchLogger.addValuesToLogging(currentHyperparameterSpace);

            // check if performance of current space is the best, if yes set current space as best space
            if (checkIfBetter(currentHyperparameterSpace.getPerformance() * currentHyperparameterSpace.getCoverage())) {
                bestHyperparameterSpace = new HyperparameterSpace(currentHyperparameterSpace);
                bestGlobalExplanations = globalExplanations;
            }
//
//            // randomize all hyperparameters
            currentHyperparameterSpace = randomizeHyperparameters(currentHyperparameterSpace);

            nrExecutions++;
        }

        randomSearchLogger.endLogging();

        // visualize best hyperparameters and the best global explanations
        visualizeBestHyperparameterSpace();
        System.out.println(anchorTabular.getVisualizer().visualizeGlobalResults(this.bestGlobalExplanations));

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

        if (performance > this.bestHyperparameterSpace.getPerformance() * this.bestHyperparameterSpace.getCoverage()) {
            return true;
        }
        return false;
    }


    private void visualizeBestHyperparameterSpace() {

        StringBuilder sb = new StringBuilder();
        for (Parameter p : this.bestHyperparameterSpace.getHyperParameters()){
            sb.append(p.getName() + ": " + p.getCurrentValue() + System.lineSeparator());
        }

        System.out.println("==== The best Hyperparameter Space is ====" + System.lineSeparator() +
                sb.toString() +
                "coverage: " + this.bestHyperparameterSpace.getCoverage() + System.lineSeparator() +
                "performance: " + this.bestHyperparameterSpace.getPerformance() + System.lineSeparator() +
                "runtime: " + this.bestHyperparameterSpace.getRuntime() + "ms");
    }

    public HyperparameterSpace getCurrentHyperparameterSpace() {
        return currentHyperparameterSpace;
    }
}
