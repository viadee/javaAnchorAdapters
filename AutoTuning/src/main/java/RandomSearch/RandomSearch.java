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

    private long terminationConditionInMin = 0;
    private HyperparameterSpace currentHyperparameterSpace;
    private HyperparameterSpace bestHyperparameterSpace;

    public RandomSearch() {
        this.currentHyperparameterSpace = new HyperparameterSpace();
        this.currentHyperparameterSpace.setRandomHyperparameterSpace();

        this.bestHyperparameterSpace = new HyperparameterSpace();
    }

    public void checkParameterSpace(double performance) {

        if (performance > bestHyperparameterSpace.getPerformance()) {
            this.bestHyperparameterSpace = this.currentHyperparameterSpace;
            this.bestHyperparameterSpace.setPerformance(performance);
        }
    }

    public HyperparameterSpace getCurrentHyperparameterSpace() {
        return currentHyperparameterSpace;
    }

    public void execute(Function<TabularInstance, Integer> classificationFunction, AnchorTabular anchorTabular, TabularInstance explainedInstance) {

        long startTime = System.currentTimeMillis();

        while (false||(System.currentTimeMillis()-startTime)<10000) {

            // randomize all hyperparameters
            getCurrentHyperparameterSpace().setRandomHyperparameterSpace();

            // init the AnchorConstructionBuilder with all hyperparameters
            final AnchorConstructionBuilder<TabularInstance> anchorBuilder = anchorTabular
                    .createDefaultBuilder(classificationFunction, explainedInstance)
                    .setTau(this.currentHyperparameterSpace.getTau().getCurrentValue())
                    .setBeamSize(this.currentHyperparameterSpace.getBeamSize().getCurrentValue());

            // execute Coverage Pick of Anchors and get result
            final List<AnchorResult<TabularInstance>> globalExplanations = new CoveragePick<>(anchorBuilder, 10,
                    Executors.newCachedThreadPool(), null)
                    .run(anchorTabular.getTabularInstances(), 20);

            // init prediction model of created global rules
            PredictionModel model = new PredictionModel(globalExplanations);

            // predict data set labels
            List<Integer> prediction = model.predict(anchorTabular.getTabularInstances());

            // check if performance of current space is the best, if yes set current space as best space
            checkParameterSpace(calcAccuracyAndCoverage(prediction, classificationFunction, anchorTabular.getTabularInstances()));
        }

        visiualizeBestHyperparameterSpace();
    }

    private static double calcAccuracyAndCoverage(List<Integer> predictedValues, Function<TabularInstance, Integer> predictFunction, TabularInstance[] trainData) {
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

    private void visiualizeBestHyperparameterSpace() {

        System.out.println("==== Imitation accuracy of the global explanations is ====" + System.lineSeparator() +
                this.bestHyperparameterSpace.getPerformance());
    }

}
