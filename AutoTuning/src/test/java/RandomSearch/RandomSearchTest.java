package RandomSearch;

import DataInitialization.DataInitializer;
import DataInitialization.impl.TitanicInitializer;
import H2OWrapper.H2OTitanicWrapper;
import LossFunctions.PerformanceMeasures;
import Parameter.DiscretizerInstantiation.*;
import org.junit.Test;

import java.io.IOException;

public class RandomSearchTest {

    final private int explainedInstanceIndex = 1;
    final private String scenario = "Test";
    final private PerformanceMeasures.Measure measure = PerformanceMeasures.Measure.F1;
    final private DataInitializer data = new TitanicInitializer();
    final private H2OTitanicWrapper function = new H2OTitanicWrapper("RandomSearchTest/GBM_9086.zip");
    final private ConfigurationSpace configurationSpace = new ConfigurationSpace(HyperparameterSpace.createDefaultHyperparameterSpace(),
            new DiscretizationSpace(
                    data,
                    new PercentileMedianDiscretizerInstantiation(),
                    new FUSINTERDiscretizerInstantiation(),
                    new EqualSizeDiscretizerInstantiation(),
                    new AmevaDiscretizerInstantiation(),
                    new MDLPDiscretizerInstantiation()));

    public RandomSearchTest() throws IOException {
    }

    @Test
    public void testLocalOptimization() {
        // RANDOM SEARCH with time condition
        RandomSearch rs = new RandomSearchBuilder()
                .setExplainedInstanceIndex(explainedInstanceIndex)
                .setConfigurationSpace(configurationSpace)
                .setClassificationFunction(function::predict)
                .setData(data)
                .setMeasure(measure)
                .setScenario(scenario)
                .setExecutionTermination(10)
                .build();

        rs.optimizeLocalExplanations();
    }

    @Test
    public void testGlobalOptimization() {
        // RANDOM SEARCH with time condition
        int[] testInstances = new int[100];
        for (int i = 0; i < testInstances.length; i++) {
            testInstances[i] = i;
        }
        RandomSearch rs = new RandomSearchBuilder()
                .setExplainedInstanceIndex(testInstances)
                .setConfigurationSpace(configurationSpace)
                .setClassificationFunction(function::predict)
                .setData(data)
                .setMeasure(measure)
                .setScenario(scenario)
                .setExecutionTermination(1)
                .build();

        rs.optimizeGlobalExplanations();
    }
}