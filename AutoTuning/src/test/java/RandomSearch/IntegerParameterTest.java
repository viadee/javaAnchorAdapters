package RandomSearch;

import Parameter.IntegerParameter;
import org.junit.Assert;
import org.junit.Test;

public class IntegerParameterTest {

    @Test
    public void searchRandom() {

        // Given
        int minValue = 0;
        int maxValue = 30;
        IntegerParameter p = new IntegerParameter("name", 0, minValue, maxValue);

        // When
        int result = p.getRandomValue();
        boolean b = result > 30 || result < 0;

        // Then
        Assert.assertFalse(b);

    }
}