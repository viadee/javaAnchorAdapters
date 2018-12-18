package de.viadee.anchorj.tabular;

import org.junit.jupiter.api.Test;
import de.viadee.anchorj.tabular.transformations.StringToDoubleTransformer;
import de.viadee.anchorj.tabular.transformations.StringToIntTransformer;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 */
class StringToIntTransformerTest {

    @Test
    void testApply() {
        Integer[] expectedValues = new Integer[3];
        expectedValues[0] = 12;
        expectedValues[1] = 243654645;
        expectedValues[2] = Integer.MAX_VALUE;
        String[] valuesAsStrings = new String[expectedValues.length];
        for (int i = 0; i < expectedValues.length; i++) {
            valuesAsStrings[i] = expectedValues[i].toString();
        }

        Integer[] transformations = new StringToIntTransformer().apply(valuesAsStrings);

        for (int i = 0; i < valuesAsStrings.length; i++) {
            assertEquals(expectedValues[i], transformations[i], "of " + i + "th element");
        }
    }

}
