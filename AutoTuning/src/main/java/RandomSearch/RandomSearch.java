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

    private long terminationConditionInMin;
    private HyperparameterSpace currentHyperparameterSpace;
    private HyperparameterSpace bestHyperparameterSpace;

    /**
     * @param terminationConditionInMin
     */
    public RandomSearch(long terminationConditionInMin) {
        this.currentHyperparameterSpace = new HyperparameterSpace();
        this.currentHyperparameterSpace.setRandomHyperparameterSpace();
        this.terminationConditionInMin = terminationConditionInMin;
        this.bestHyperparameterSpace = new HyperparameterSpace();
    }

    /**
     * @param performance
     */
    public void checkParameterSpace(double performance) {

        if (performance > bestHyperparameterSpace.getPerformance()) {
            this.bestHyperparameterSpace = this.currentHyperparameterSpace;
            this.bestHyperparameterSpace.setPerformance(performance);
        }
    }


    /**
     * @param classificationFunction
     * @param anchorBuilder
     * @param anchorTabular
     */
    public void execute(Function<TabularInstance, Integer> classificationFunction, AnchorConstructionBuilder<TabularInstance> anchorBuilder, AnchorTabular anchorTabular) {

        long startTime = System.currentTimeMillis();

        while ((System.currentTimeMillis() - startTime) < (this.terminationConditionInMin * 60000)) {

            // randomize all hyperparameters
            this.currentHyperparameterSpace.setRandomHyperparameterSpace();

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            // set all hyperparameters
            anchorBuilder
                    .setTau(this.currentHyperparameterSpace.getTau().getCurrentValue())
                    .setBeamSize(this.currentHyperparameterSpace.getBeamSize().getCurrentValue())
                    .setDelta(this.currentHyperparameterSpace.getDelta().getCurrentValue())
                    .setEpsilon(this.currentHyperparameterSpace.getEpsilon().getCurrentValue())
                    .setTauDiscrepancy(this.currentHyperparameterSpace.getTauDiscrepancy().getCurrentValue())
                    .setInitSampleCount(this.currentHyperparameterSpace.getInitSampleCount().getCurrentValue());

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
            checkParameterSpace(calcPerformance(prediction, classificationFunction, anchorTabular.getTabularInstances()));
        }

        visualizeBestHyperparameterSpace();
    }

    /**
     * @param predictedValues
     * @param predictFunction
     * @param trainData
     * @return
     */
    private static double calcPerformance(List<Integer> predictedValues, Function<TabularInstance, Integer> predictFunction, TabularInstance[] trainData) {
        int predInstances = 0;
        int matches = 0;
        for (int i = 0; i < trainData.length; i++) {
            if (!predictedValues.get(i).equals(-1)) {
                predInstances++;
                if (predictFunction.apply(trainData[i]).equals(predictedValues.get(i)))
                    matches++;
            }
        }

        double accuracy = (double) matches / (double) predInstances;
        double coverage = (double) predInstances / (double) predictedValues.size();

        System.out.println("==== Imitation accuracy of the global explanations is ====" + System.lineSeparator() +
                accuracy);
        System.out.println("==== Total Coverage of the global explanations is ====" + System.lineSeparator() +
                coverage);

        return accuracy * coverage;
    }

    private void visualizeBestHyperparameterSpace() {

        System.out.println("==== The best Hyperparameter Space is ====" + System.lineSeparator() +
                "Tau: " + this.bestHyperparameterSpace.getTau().getCurrentValue() + System.lineSeparator() +
                "BeamSize: " + this.bestHyperparameterSpace.getBeamSize().getCurrentValue() + System.lineSeparator() +
                "Delta: " + this.bestHyperparameterSpace.getDelta().getCurrentValue() + System.lineSeparator() +
                "Epsilon: " + this.bestHyperparameterSpace.getEpsilon().getCurrentValue() + System.lineSeparator() +
                "TauDiscrepancy: " + this.bestHyperparameterSpace.getTauDiscrepancy().getCurrentValue() + System.lineSeparator() +
                "InitSampleCount: " + this.bestHyperparameterSpace.getInitSampleCount().getCurrentValue() + System.lineSeparator() +
                "Performance: " + this.bestHyperparameterSpace.getPerformance() + System.lineSeparator() +
                "Runtime: " + this.bestHyperparameterSpace.getRuntime() + "ms");
    }

    public HyperparameterSpace getCurrentHyperparameterSpace() {
        return currentHyperparameterSpace;
    }

}
