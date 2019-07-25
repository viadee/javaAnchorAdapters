package Parameter;

import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.stream.Stream;

import static org.junit.Assert.*;

public class CategoricalParameterTest {

    @Test
    public void testSearchRandomString() {
        // Given
        String[] test = {"eins", "zwei", "drei"};
        CategoricalParameter categoricalParameter = new CategoricalParameter("test", test);

        for (int i = 0; i < 10; i++) {
            // When
            categoricalParameter.searchRandom();

            // Then
            Assert.assertTrue(Arrays.stream(test).anyMatch(t -> t.equals(categoricalParameter.getCurrentValue())));
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void testSearchRandomEmpty() {
        // Given
        String[] test = {};
        CategoricalParameter categoricalParameter = new CategoricalParameter("test", test);
    }
}