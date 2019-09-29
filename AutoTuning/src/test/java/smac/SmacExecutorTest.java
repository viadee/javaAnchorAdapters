package smac;

import dataInitialization.DataInitializer;
import dataInitialization.impl.TitanicInitializer;
import H2OWrapper.H2OTitanicWrapper;
import evaluationMetrics.PerformanceMeasures;
import configurationSpace.discretizerInstantiation.*;
import configurationSpace.ConfigurationSpace;
import configurationSpace.DiscretizationSpace;
import configurationSpace.HyperparameterSpace;
import randomSearch.Logger;
import smac.impl.CoveragePickTAE;
import smac.impl.LocalAnchorsTAE;
import org.junit.Test;

import java.io.IOException;

public class SmacExecutorTest {

    // Given
    final private int explainedInstanceIndex = 1;
    final private String scenario = "Test";
    final private PerformanceMeasures.Measure measure = PerformanceMeasures.Measure.F1;
    final private DataInitializer data = new TitanicInitializer();
    final private H2OTitanicWrapper function = new H2OTitanicWrapper("Titanic/GBM_9086.zip");
    final private ConfigurationSpace configurationSpace = new ConfigurationSpace(HyperparameterSpace.createDefaultHyperparameterSpace(),
            new DiscretizationSpace(
                    data,
                    new PercentileMedianDiscretizerInstantiation(),
                    new FUSINTERDiscretizerInstantiation(),
                    new EqualSizeDiscretizerInstantiation(),
                    new AmevaDiscretizerInstantiation(),
                    new MDLPDiscretizerInstantiation()));

    public SmacExecutorTest() throws IOException {
    }


    @Test(expected = IllegalArgumentException.class)
    public void testEmptyTAERun() {

        // Then
        new SmacExecutorBuilder()
                .setScenario(scenario)
                .setConfigurationSpace(configurationSpace)
                .setTotalNumRunsLimit(1)
                .build();
    }

    @Test
    public void testRunLocal() {
        Logger logger = new Logger("smac-output",scenario, configurationSpace, PerformanceMeasures.Measure.PRECISION);
        LocalAnchorsTAE tae = new LocalAnchorsTAE(explainedInstanceIndex, function::predict, data, logger);
        SmacExecutor exe = new SmacExecutorBuilder()
                .setScenario(scenario)
                .setConfigurationSpace(configurationSpace)
                .setTargetAlgorithmEvaluator(tae)
                .setTotalNumRunsLimit(10)
                .build();

        // When
        exe.run();
        logger.endLogging();
    }

    @Test
    public void testRunGlobal() {
        Logger logger = new Logger("smac-output",scenario, configurationSpace, PerformanceMeasures.Measure.RECALL);
        int[] instances = new int[100];
        for (int i = 0; i < instances.length; i++) {
            instances[i] = i;
        }
        CoveragePickTAE tae = new CoveragePickTAE(instances, function::predict, data, PerformanceMeasures.Measure.RECALL,logger);
        SmacExecutor exe = new SmacExecutorBuilder()
                .setScenario(scenario)
                .setConfigurationSpace(configurationSpace)
                .setTargetAlgorithmEvaluator(tae)
                .setTotalNumRunsLimit(2)
                .build();

        // When
        exe.run();
        logger.endLogging();
    }
}