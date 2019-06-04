package RandomSearch;

import org.junit.Assert;
import org.junit.Test;
import play.mvc.WebSocket;

import static org.junit.Assert.*;

public class IntegerParameterTest {

    @Test
    public void searchRandom() {

        // Given
        int minValue = 0;
        int maxValue = 30;
        IntegerParameter p = new IntegerParameter("name",minValue, maxValue);

        // When
        p.searchRandom();
        int result = p.getCurrentValue();
        boolean b = result > 30 || result < 0;

        // Then
        Assert.assertFalse(b);

    }
}