package RandomSearch;

import LossFunctions.Accuracy.PredictionModel;
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
    public void execute(Function<TabularInstance, Integer> classificationFunction, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular) {

        long startTime = System.currentTimeMillis();
        int nrExecutions = 0;

        while ((System.currentTimeMillis() - startTime) < (this.terminationConditionInSec * 1000) || nrExecutions < this.terminationConditionNrEx) {

            // randomize all hyperparameters
            this.currentHyperparameterSpace.setRandomHyperparameterSpace();

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            // set all hyperparameters
            // for starting with the default values
            if (this.startWithDefault){
                anchorBuilder
                        .setTau(this.currentHyperparameterSpace.getTau().getDefaultValue())
                        .setBeamSize(this.currentHyperparameterSpace.getBeamSize().getDefaultValue())
                        .setDelta(this.currentHyperparameterSpace.getDelta().getDefaultValue())
                        .setEpsilon(this.currentHyperparameterSpace.getEpsilon().getDefaultValue())
                        .setTauDiscrepancy(this.currentHyperparameterSpace.getTauDiscrepancy().getDefaultValue())
                        .setInitSampleCount(this.currentHyperparameterSpace.getInitSampleCount().getDefaultValue());
                this.startWithDefault = false;
            } else {
                anchorBuilder
                        .setTau(this.currentHyperparameterSpace.getTau().getCurrentValue())
                        .setBeamSize(this.currentHyperparameterSpace.getBeamSize().getCurrentValue())
                        .setDelta(this.currentHyperparameterSpace.getDelta().getCurrentValue())
                        .setEpsilon(this.currentHyperparameterSpace.getEpsilon().getCurrentValue())
                        .setTauDiscrepancy(this.currentHyperparameterSpace.getTauDiscrepancy().getCurrentValue())
                        .setInitSampleCount(this.currentHyperparameterSpace.getInitSampleCount().getCurrentValue());
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

            // check if performance of current space is the best, if yes set current space as best space
            this.currentHyperparameterSpace.calcPerformance(prediction, classificationFunction, anchorTabular.getTabularInstances());
            if (checkIfBetter(this.currentHyperparameterSpace.getPrecision() * this.currentHyperparameterSpace.getCoverage())) {
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

        if (performance > this.bestHyperparameterSpace.getPrecision() * this.bestHyperparameterSpace.getCoverage()) {
            return true;
        }
        return false;
    }


    private void visualizeBestHyperparameterSpace() {

        System.out.println("==== The best Hyperparameter Space is ====" + System.lineSeparator() +
                "Tau: " + this.bestHyperparameterSpace.getTau().getCurrentValue() + System.lineSeparator() +
                "BeamSize: " + this.bestHyperparameterSpace.getBeamSize().getCurrentValue() + System.lineSeparator() +
                "Delta: " + this.bestHyperparameterSpace.getDelta().getCurrentValue() + System.lineSeparator() +
                "Epsilon: " + this.bestHyperparameterSpace.getEpsilon().getCurrentValue() + System.lineSeparator() +
                "TauDiscrepancy: " + this.bestHyperparameterSpace.getTauDiscrepancy().getCurrentValue() + System.lineSeparator() +
                "InitSampleCount: " + this.bestHyperparameterSpace.getInitSampleCount().getCurrentValue() + System.lineSeparator() +
                "Precision: " + this.bestHyperparameterSpace.getPrecision() + System.lineSeparator() +
                "Coverage: " + this.bestHyperparameterSpace.getCoverage() + System.lineSeparator() +
                "Performance: " + this.bestHyperparameterSpace.getPrecision() * this.bestHyperparameterSpace.getCoverage() + System.lineSeparator() +
                "Runtime: " + this.bestHyperparameterSpace.getRuntime() + "ms");
    }

    public HyperparameterSpace getCurrentHyperparameterSpace() {
        return currentHyperparameterSpace;
    }

}
