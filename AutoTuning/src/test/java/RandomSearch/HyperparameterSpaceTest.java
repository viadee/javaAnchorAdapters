package RandomSearch;

import Parameter.IntegerParameter;
import Parameter.Parameter;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class HyperparameterSpaceTest {

    private final String name = "Test";
    private final int defaultValue = 5;
    private final int minValue = 1;
    private final int maxValue = 50;
    IntegerParameter parameter = new IntegerParameter(name, defaultValue, minValue, maxValue);

    @Test
    public void testAddParameter() {
        // Given
        HyperparameterSpace hs = HyperparameterSpace.createDefaultHyperparameterSpace();

        // When
        hs.addParameter(parameter);

        // Then
        Assert.assertSame(defaultValue, ((IntegerParameter) hs.getParameterByName(name)).getDefaultValue());
        Assert.assertSame(minValue, ((IntegerParameter) hs.getParameterByName(name)).getMinValue());
        Assert.assertSame(maxValue, ((IntegerParameter) hs.getParameterByName(name)).getMaxValue());
    }

    @Test
    public void testGetParameterByName() {
        // Given
        HyperparameterSpace hs = new HyperparameterSpace(Arrays.asList(parameter));

        // When
        IntegerParameter p = (IntegerParameter) hs.getParameterByName(name);

        // Then
        Assert.assertSame(defaultValue, p.getDefaultValue());
        Assert.assertSame(minValue, p.getMinValue());
        Assert.assertSame(maxValue, p.getMaxValue());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGetParameterByNameNotFound() {
        // Given
        HyperparameterSpace hs = new HyperparameterSpace(Arrays.asList(parameter));

        // Then
        hs.getParameterByName("Name not found");
    }

    @Test
    public void getHyperParameters() {
        // Given
        HyperparameterSpace hs = new HyperparameterSpace(Arrays.asList(parameter));

        // When
        List<Parameter> result = hs.getHyperParameters();

        // Then
        Assert.assertSame(defaultValue, ((IntegerParameter) result.get(0)).getDefaultValue());
        Assert.assertSame(minValue, ((IntegerParameter) result.get(0)).getMinValue());
        Assert.assertSame(maxValue, ((IntegerParameter) result.get(0)).getMaxValue());
    }
}