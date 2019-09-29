package randomSearch;

import configurationSpace.ConfigurationSpace;
import configurationSpace.HyperparameterSpace;
import configurationSpace.NumericalParameter;
import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfigurationSpace;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;

public class ConfigurationSpaceTest {

    private AnchorTabular setTabular() {
        try {
            return new AnchorTabularBuilderByName()
                    .addTargetColumn(IntegerColumn.fromStringInput("Survived"))
                    .addColumn(DoubleColumn.fromStringInput("Age", -1, 5))
                    .build(ClassLoader.getSystemResourceAsStream("train.csv"));
        } catch (IOException e) {
            throw new IllegalArgumentException("No file found with such name");
        }
    }

    @Test
    public void testToParameterConfigurationSpaceOnlyHyperparameters() {
        // Given
        ConfigurationSpace configurationSpace = new ConfigurationSpace(HyperparameterSpace.createDefaultHyperparameterSpace());

        // When
        ParameterConfigurationSpace parameterConfigurationSpace = configurationSpace.toParameterConfigurationSpace();

        // Then
        Assert.assertEquals(
                (((NumericalParameter)configurationSpace.getHyperparameterSpace().getParameterByName("tau")).getDefaultValue().doubleValue()),
                Double.valueOf(parameterConfigurationSpace.getDefaultConfiguration().get("tau")),
                0D);
    }
}