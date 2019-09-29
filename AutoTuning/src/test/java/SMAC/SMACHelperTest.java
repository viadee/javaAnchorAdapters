package SMAC;

import DataInitialization.DataInitializer;
import DataInitialization.impl.HellaInitializer;
import DataInitialization.impl.TitanicInitializer;
import H2OWrapper.H2OHellaWrapper;
import H2OWrapper.H2OTitanicWrapper;
import LossFunctions.PerformanceMeasures;
import Parameter.DiscretizerInstantiation.*;
import RandomSearch.ConfigurationSpace;
import RandomSearch.DiscretizationSpace;
import RandomSearch.HyperparameterSpace;
import RandomSearch.Logger;
import SMAC.impl.CoveragePickTAE;
import SMAC.impl.LocalAnchorsTAE;
import ca.ubc.cs.beta.aeatk.objectives.RunObjective;
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
import java.util.Arrays;

public class SMACHelperTest {

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

    public SMACHelperTest() throws IOException {
    }


    @Test(expected = IllegalArgumentException.class)
    public void testEmptyTAERun() {

        // Then
        new SMACBuilder()
                .setScenario(scenario)
                .setConfigurationSpace(configurationSpace)
                .setTotalNumRunsLimit(1)
                .build();
    }

    @Test
    public void testRunLocal() {
        Logger logger = new Logger("smac-output",scenario, configurationSpace, PerformanceMeasures.Measure.PRECISION);
        LocalAnchorsTAE tae = new LocalAnchorsTAE(explainedInstanceIndex, function::predict, data, logger);
        SMACHelper exe = new SMACBuilder()
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
        SMACHelper exe = new SMACBuilder()
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