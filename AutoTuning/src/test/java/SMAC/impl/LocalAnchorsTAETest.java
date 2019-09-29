package SMAC.impl;

import DataInitialization.impl.TitanicInitializer;
import LossFunctions.PerformanceMeasures;
import RandomSearch.Logger;
import ca.ubc.cs.beta.aeatk.algorithmexecutionconfiguration.AlgorithmExecutionConfiguration;
import ca.ubc.cs.beta.aeatk.algorithmrunconfiguration.AlgorithmRunConfiguration;
import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfigurationSpace;
import ca.ubc.cs.beta.aeatk.probleminstance.ProblemInstance;
import ca.ubc.cs.beta.aeatk.probleminstance.ProblemInstanceSeedPair;
import de.viadee.xai.anchor.adapter.classifiers.TabularRandomForestClassifier;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceEmptyTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceNonEmptyTransformer;
import org.junit.Test;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Arrays;


public class LocalAnchorsTAETest {

    private static AnchorTabular createTabular() {

        InputStream trainingDataStream = ClassLoader.getSystemResourceAsStream("Titanic/train.csv");

        if (trainingDataStream == null)
            throw new RuntimeException("Could not load data");

        try {
            return new AnchorTabularBuilderByName()
                    .setDoBalance(false)
                    .addTargetColumn(IntegerColumn.fromStringInput("Survived"))
                    .addColumn(IntegerColumn.fromStringInput("Pclass"))
                    .addColumn(new StringColumn("Name"))
                    .addColumn(new StringColumn("Sex"))
                    .addColumn(DoubleColumn.fromStringInput("Age", -1, 5))
                    .addColumn(IntegerColumn.fromStringInput("SibSp"))
                    .addColumn(IntegerColumn.fromStringInput("Parch"))
                    .addColumn(DoubleColumn.fromStringInput("Fare", -1, 6))
                    .addColumn(new StringColumn("Cabin", Arrays.asList(new ReplaceEmptyTransformer(false), new ReplaceNonEmptyTransformer(true)), null))
                    .addColumn(new StringColumn("Embarked"))
                    .build(trainingDataStream, false, false);
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("Could not read data");
        }
    }

    @Test
    public void evaluateRun() {

        ParameterConfigurationSpace space = new ParameterConfigurationSpace(new StringReader("tau real [0.0, 1.0][0.8]"));
        AlgorithmExecutionConfiguration execConfig = new AlgorithmExecutionConfiguration("", "", space, false, false, 0);
        AlgorithmRunConfiguration rc = new AlgorithmRunConfiguration(new ProblemInstanceSeedPair(new ProblemInstance("test"), 2), 25.0, space.getDefaultConfiguration(), execConfig);

        final AnchorTabular tabular = this.createTabular();

        final TabularRandomForestClassifier randomForestModel = new TabularRandomForestClassifier(100, true);
        randomForestModel.fit(tabular.getTabularInstances());

        LocalAnchorsTAE tae = new LocalAnchorsTAE(1, randomForestModel, new TitanicInitializer(), new Logger("Test"));
        tae.evaluateRun(rc);
    }
}