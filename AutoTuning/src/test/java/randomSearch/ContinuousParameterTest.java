package randomSearch;

import configurationSpace.ContinuousParameter;
import org.junit.Assert;
import org.junit.Test;

public class ContinuousParameterTest {

    @Test
    public void searchRandom() {

        // Given
        double minValue = 0.5;
        double maxValue = 0.95;
        ContinuousParameter p = new ContinuousParameter("test", 0, minValue, maxValue);

        // When
        double result = p.getRandomValue();
        boolean b = result < 0.5 || result > 0.95;

        // Then
        Assert.assertFalse(b);
    }
}