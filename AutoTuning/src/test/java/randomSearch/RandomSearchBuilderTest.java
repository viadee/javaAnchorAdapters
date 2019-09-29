package randomSearch;

import evaluationMetrics.PerformanceMeasures;
import configurationSpace.ConfigurationSpace;
import configurationSpace.HyperparameterSpace;
import de.viadee.xai.anchor.adapter.classifiers.TabularRandomForestClassifier;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import org.junit.Test;

import java.io.IOException;
import java.util.function.Function;

public class RandomSearchBuilderTest {

    private AnchorTabular anchorTabular = setTabular();
    private Function<TabularInstance, Integer> classificationFunction = fitModel();
    private AnchorConstructionBuilder<TabularInstance> anchorBuilder = anchorTabular.createDefaultBuilder(classificationFunction, anchorTabular.getTabularInstances()[1]);
    private ConfigurationSpace configurationSpace = new ConfigurationSpace(HyperparameterSpace.createDefaultHyperparameterSpace());
    private long timeTermination = 200;
    private int executionTermination = 10;
    private String scenario = "Test";
    private PerformanceMeasures.Measure measure = PerformanceMeasures.Measure.ACCURACY;
    private boolean startWithDefault = true;


    private AnchorTabular setTabular() {
        try {
           return new AnchorTabularBuilderByName()
                   .addTargetColumn(IntegerColumn.fromStringInput("Survived"))
                   .addColumn(IntegerColumn.fromStringInput("Pclass"))
                   .build(ClassLoader.getSystemResourceAsStream("train.csv"));
        } catch (IOException e) {
            throw new IllegalArgumentException("No file found with such name");
        }
    }

    private Function<TabularInstance, Integer> fitModel() {
        TabularRandomForestClassifier randomForestModel = new TabularRandomForestClassifier(100, true);
        randomForestModel.fit(anchorTabular.getTabularInstances());
        return  randomForestModel;
    }

    @Test
    public void testRandomSearchBuilder() {
        new RandomSearchBuilder()
                .setExplainedInstanceIndex(0)
                .setConfigurationSpace(configurationSpace)
                .setMeasure(measure)
                .setClassificationFunction(classificationFunction)
                .setExecutionTermination(executionTermination)
                .setTimeTermination(timeTermination)
                .setScenario(scenario)
                .notStartWithDefault()
                .build();

    }

    @Test
    public void testRandomSearchBuilderWithConstructor() {
        new RandomSearchBuilder()
                .createDefaultBuilder(anchorTabular, classificationFunction, configurationSpace, 0);
    }


}