import LossFunctions.PerformanceMeasures;
import RandomSearch.RandomSearch;
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
        final AnchorTabular anchorTabular = HellaDataset.createTabularTrainingDefinition();
        final AnchorTabular anchorTabularTest = HellaDataset.createTabularTestDefinition();

        // Obtain a second suitable model (RandomForest). Train it ourselves this time.
        final TabularRandomForestClassifier randomForestModel = new TabularRandomForestClassifier(100);
        randomForestModel.fit(anchorTabular.getTabularInstances());

        final H2OHellaWrapper h2oModel = new H2OHellaWrapper();

        // Print the model's test data accuracy
        //outputTestsetAccuracy("RandomForest", randomForestModel);

        // Pick instance to be explained
        // Next pick specific instance (countess or patrick dooley)
        final TabularInstance explainedInstance = anchorTabular.getTabularInstances()[1704];

        final AnchorConstructionBuilder<TabularInstance> anchorBuilder = anchorTabularTest
                .createDefaultBuilder(h2oModel::predict, explainedInstance);

//        anchorBuilder.setTau(0.77).setBeamSize(11).setDelta(0.11).setEpsilon(0.41).setTauDiscrepancy(0.09).setInitSampleCount(3);

        // RANDOM SEARCH with time condintion
        RandomSearch rs = new RandomSearch("Hella_ASN_90", anchorBuilder, anchorTabular, 200, true);
        rs.optimizeExplanations(h2oModel::predict, anchorTabularTest, PerformanceMeasures.Measure.PRECISION, false);

        // SMAC with condition
//        HyperparameterSpace hyperparameterSpace = new HyperparameterSpace();
//        SMACHelper smacHelper = new SMACHelper("Titanic", hyperparameterSpace, "anchorsOptimizer.jar", "C:\\Users\\B96\\IdeaProjects\\javaAnchorAdapters\\AutoTuning\\out\\artifacts\\anchorsOptimizer_jar", 3);
//        smacHelper.run();

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
