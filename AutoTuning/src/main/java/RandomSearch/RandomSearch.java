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
        this.currentHyperparameterSpace = new HyperparameterSpace();
        if (!startWithDefault) {
            this.currentHyperparameterSpace = new HyperparameterSpace();
        }
        this.terminationConditionInSec = terminationConditionInSec;
        this.terminationConditionNrEx = terminationConditionNrEx;
        this.bestHyperparameterSpace = new HyperparameterSpace();
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

        while ((System.currentTimeMillis() - startTime) < (this.terminationConditionInSec * 1000) || nrExecutions < this.terminationConditionNrEx) {

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            // set all hyperparameters

            anchorBuilder
                    .setTau(this.currentHyperparameterSpace.getParameterByName("tau").getCurrentValue().doubleValue())
                    .setBeamSize(this.currentHyperparameterSpace.getParameterByName("beamsize").getCurrentValue().intValue())
                    .setDelta(this.currentHyperparameterSpace.getParameterByName("delta").getCurrentValue().doubleValue())
                    .setEpsilon(this.currentHyperparameterSpace.getParameterByName("epsilon").getCurrentValue().doubleValue())
                    .setTauDiscrepancy(this.currentHyperparameterSpace.getParameterByName("tauDiscrepancy").getCurrentValue().doubleValue())
                    .setInitSampleCount(this.currentHyperparameterSpace.getParameterByName("initSampleCount").getCurrentValue().intValue());


            // execute Coverage Pick of Anchors and get result
            final List<AnchorResult<TabularInstance>> globalExplanations = new CoveragePick<>(anchorBuilder, 10,
                    Executors.newCachedThreadPool(), null)
                    .run(anchorTabular.getTabularInstances(), 20);

//            // set runtime of current Anchors run
            this.currentHyperparameterSpace.setRuntime(System.currentTimeMillis() - runtimeStart);
//
//            // init prediction model of created global rules
            PredictionModel model = new PredictionModel(globalExplanations);
//
//            // predict data set labels
            List<Integer> prediction = model.predict(anchorTabular.getTabularInstances());
//
//            // init performance measure
            PerformanceMeasures performanceMeasures = new PerformanceMeasures(prediction, classificationFunction, anchorTabular.getTabularInstances());
            this.currentHyperparameterSpace.setPerformance(performanceMeasures.calcMeasure(measure));
            this.currentHyperparameterSpace.setCoverage(performanceMeasures.getCoverage());
//
//            // check if performance of current space is the best, if yes set current space as best space
            if (checkIfBetter(this.currentHyperparameterSpace.getPerformance() * this.currentHyperparameterSpace.getCoverage())) {
                this.bestHyperparameterSpace = new HyperparameterSpace(this.currentHyperparameterSpace);
                this.bestGlobalExplanations = globalExplanations;
            }
//
//            // randomize all hyperparameters
            this.currentHyperparameterSpace = randomizeHyperparameters(this.currentHyperparameterSpace);

            nrExecutions++;
        }

//        RandomSearchLogger randomSearchLogger = new RandomSearchLogger(this.scenario, this.bestHyperparameterSpace);

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
