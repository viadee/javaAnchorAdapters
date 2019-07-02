package RandomSearch;

import LossFunctions.Accuracy.PredictionModel;
import LossFunctions.PerformanceMeasures;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;

import java.util.List;
import java.util.concurrent.Executors;
import java.util.function.Function;


public class RandomSearch {

    private long terminationConditionInSec;
    private int terminationConditionNrEx;
    private HyperparameterSpace currentHyperparameterSpace;
    private HyperparameterSpace bestHyperparameterSpace;
    private List<AnchorResult<TabularInstance>> bestGlobalExplanations;
    private boolean startWithDefault;

    /**
     * @param terminationConditionInSec
     */
    private RandomSearch(long terminationConditionInSec, int terminationConditionNrEx, boolean startWithDefault) {
        this.currentHyperparameterSpace = new HyperparameterSpace();
        this.currentHyperparameterSpace.setRandomHyperparameterSpace();
        this.terminationConditionInSec = terminationConditionInSec;
        this.terminationConditionNrEx = terminationConditionNrEx;
        this.startWithDefault = startWithDefault;
        this.bestHyperparameterSpace = new HyperparameterSpace();
    }

    public RandomSearch(long terminationConditionInSec, boolean startWithDefault) {
        this(terminationConditionInSec, 0, startWithDefault);
    }

    public RandomSearch(int terminationConditionNrEx, boolean startWithDefault) {
        this(0, terminationConditionNrEx, startWithDefault);
    }

    public RandomSearch(long terminationConditionInSec) {
        this(terminationConditionInSec, 0, true);
    }

    public RandomSearch(int terminationConditionNrEx) {
        this(0, terminationConditionNrEx, true);
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

            // randomize all hyperparameters
            this.currentHyperparameterSpace.setRandomHyperparameterSpace();

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            // set all hyperparameters
            // for starting with the default values
            if (this.startWithDefault) {
                anchorBuilder
                        .setTau(this.currentHyperparameterSpace.getParameterByName("tau").getDefaultValue().doubleValue())
                        .setBeamSize(this.currentHyperparameterSpace.getParameterByName("beamsize").getDefaultValue().intValue())
                        .setDelta(this.currentHyperparameterSpace.getParameterByName("delta").getDefaultValue().doubleValue())
                        .setEpsilon(this.currentHyperparameterSpace.getParameterByName("epsilon").getDefaultValue().doubleValue())
                        .setTauDiscrepancy(this.currentHyperparameterSpace.getParameterByName("tauDiscrepancy").getDefaultValue().doubleValue())
                        .setInitSampleCount(this.currentHyperparameterSpace.getParameterByName("initSampleCount").getDefaultValue().intValue());
                this.startWithDefault = false;
            } else {
                anchorBuilder
                        .setTau(this.currentHyperparameterSpace.getParameterByName("tau").getCurrentValue().doubleValue())
                        .setBeamSize(this.currentHyperparameterSpace.getParameterByName("beamsize").getCurrentValue().intValue())
                        .setDelta(this.currentHyperparameterSpace.getParameterByName("delta").getCurrentValue().doubleValue())
                        .setEpsilon(this.currentHyperparameterSpace.getParameterByName("epsilon").getCurrentValue().doubleValue())
                        .setTauDiscrepancy(this.currentHyperparameterSpace.getParameterByName("tauDiscrepancy").getCurrentValue().doubleValue())
                        .setInitSampleCount(this.currentHyperparameterSpace.getParameterByName("initSampleCount").getCurrentValue().intValue());
            }

            // execute Coverage Pick of Anchors and get result
            final List<AnchorResult<TabularInstance>> globalExplanations = new CoveragePick<>(anchorBuilder, 10,
                    Executors.newCachedThreadPool(), null)
                    .run(anchorTabular.getTabularInstances(), 20);

            // set runtime of current Anchors run
            this.currentHyperparameterSpace.setRuntime(System.currentTimeMillis() - runtimeStart);

            // init prediction model of created global rules
            PredictionModel model = new PredictionModel(globalExplanations);

            // predict data set labels
            List<Integer> prediction = model.predict(anchorTabular.getTabularInstances());

            // init performance measure
            PerformanceMeasures performanceMeasures = new PerformanceMeasures(prediction, classificationFunction, anchorTabular.getTabularInstances());
            this.currentHyperparameterSpace.setPerformance(performanceMeasures.calcMeasure(measure));
            this.currentHyperparameterSpace.setCoverage(performanceMeasures.getCoverage());

            // check if performance of current space is the best, if yes set current space as best space
            if (checkIfBetter(this.currentHyperparameterSpace.getPerformance() * this.currentHyperparameterSpace.getCoverage())) {
                this.bestHyperparameterSpace = this.currentHyperparameterSpace;
                this.bestGlobalExplanations = globalExplanations;
            }

            nrExecutions++;
        }

        // visualize best hyperparameters and the best global explanations
        visualizeBestHyperparameterSpace();
        System.out.println(anchorTabular.getVisualizer().visualizeGlobalResults(this.bestGlobalExplanations));

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

        System.out.println("==== The best Hyperparameter Space is ====" + System.lineSeparator() +
                "Tau: " + this.bestHyperparameterSpace.getParameterByName("tau").getCurrentValue() + System.lineSeparator() +
                "BeamSize: " + this.bestHyperparameterSpace.getParameterByName("beamsize").getCurrentValue() + System.lineSeparator() +
                "Delta: " + this.bestHyperparameterSpace.getParameterByName("delta").getCurrentValue() + System.lineSeparator() +
                "Epsilon: " + this.bestHyperparameterSpace.getParameterByName("epsilon").getCurrentValue() + System.lineSeparator() +
                "TauDiscrepancy: " + this.bestHyperparameterSpace.getParameterByName("tauDiscrepancy").getCurrentValue() + System.lineSeparator() +
                "InitSampleCount: " + this.bestHyperparameterSpace.getParameterByName("initSampleCount").getCurrentValue() + System.lineSeparator() +
                "Coverage: " + this.bestHyperparameterSpace.getCoverage() + System.lineSeparator() +
                "Performance: " + this.bestHyperparameterSpace.getPerformance() + System.lineSeparator() +
                "Runtime: " + this.bestHyperparameterSpace.getRuntime() + "ms");
    }

    public HyperparameterSpace getCurrentHyperparameterSpace() {
        return currentHyperparameterSpace;
    }

}
