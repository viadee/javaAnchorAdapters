import RandomSearch.RandomSearch;
import ca.ubc.cs.beta.smac.executors.SMACExecutor;
import de.viadee.xai.anchor.adapter.classifiers.TabularRandomForestClassifier;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.function.Function;


public class main {

    public static void main(String[] args) throws IOException {
        // Load dataset and its description
        final AnchorTabular anchorTabular = TitanicDataset.createTabularTrainingDefinition();

        // Obtain a second suitable model (RandomForest). Train it ourselves this time.
        final TabularRandomForestClassifier randomForestModel = new TabularRandomForestClassifier(100);
        randomForestModel.fit(anchorTabular.getTabularInstances());

        // Print the model's test data accuracy
        outputTestsetAccuracy("RandomForest", randomForestModel);

        // Pick instance to be explained
        // Next pick specific instance (countess or patrick dooley)
        final TabularInstance explainedInstance = anchorTabular.getTabularInstances()[600];

        final AnchorConstructionBuilder<TabularInstance> anchorBuilder = anchorTabular
                .createDefaultBuilder(randomForestModel, explainedInstance);

        // RANDOM SEARCH with time condintion
        RandomSearch rs = new RandomSearch(1);
        rs.execute(randomForestModel, anchorBuilder, anchorTabular);

        SMACExecutor smacExecutor = new SMACExecutor();

        smacExecutor.main(null);


        System.out.println("-- COMPLETE --");
    }

    private static void printLocalExplanationResult(TabularInstance instance, AnchorTabular tabular,
                                                    AnchorConstructionBuilder<TabularInstance> builder) {
        final AnchorResult<TabularInstance> anchor = builder
                .build()
                .constructAnchor();

        System.out.println("====Explained instance====" + System.lineSeparator() +
                tabular.getVisualizer().visualizeInstance(anchor.getInstance()));

        System.out.println("====Result====" + System.lineSeparator() +
                tabular.getVisualizer().visualizeResult(anchor));
    }

    private static void printGlobalExplanationResult(AnchorTabular tabular,
                                                     AnchorConstructionBuilder<TabularInstance> builder) {
        // Use the CoveragePick algorithm to create global explanations
        final List<AnchorResult<TabularInstance>> globalExplanations = new CoveragePick<>(builder, 10,
                Executors.newCachedThreadPool(), null)
                .run(tabular.getTabularInstances(), 20);

        System.out.println(tabular.getVisualizer().visualizeGlobalResults(globalExplanations));
    }


//    private static double calcAccuracyAndCoverage(List<Integer> predictedValues, Function<TabularInstance, Integer> predictFunction) {
//        final TabularInstance[] trainData = TitanicDataset.createTabularTrainingDefinition().getTabularInstances();
//        int predInstances = 0;
//        int matches = 0;
//        for (int i = 0; i < trainData.length; i++) {
//            if (!predictedValues.get(i).equals(-1)) {
//                predInstances++;
//                if (predictFunction.apply(trainData[i]).equals(predictedValues.get(i)))
//                    matches++;
//            }
//        }
//
//        double accuracy = (double) matches / (double) predInstances;
//        double coverage = (double) predInstances / (double) predictedValues.size();
//
//        System.out.println("==== Imitation accuracy of the global explanations is ====" + System.lineSeparator() +
//                accuracy);
//        System.out.println("==== Total Coverage of the global explanations is ====" + System.lineSeparator() +
//                coverage);
//
//        return accuracy * coverage;
//    }


    private static void outputTestsetAccuracy(String name, Function<TabularInstance, Integer> predictFunction) {
        final TabularInstance[] testData = TitanicDataset.createTabularTestDefinition().getTabularInstances();
        final int[] actualTestLabels = TitanicDataset.readTestLabels();
        int matches = 0;
        for (int i = 0; i < testData.length; i++) {
            if (predictFunction.apply(testData[i]).equals(actualTestLabels[i]))
                matches++;
        }

        System.out.println("====" + name + " Testset Accuracy====" + System.lineSeparator() +
                matches / (double) testData.length);
    }

}
