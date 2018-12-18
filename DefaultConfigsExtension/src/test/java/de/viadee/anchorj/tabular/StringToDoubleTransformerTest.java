package de.viadee.anchorj.tabular;

import org.junit.jupiter.api.Test;
import de.viadee.anchorj.tabular.transformations.StringToDoubleTransformer;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 *
 */
class StringToDoubleTransformerTest {

    @Test
    void testApply() {
        Double[] expectedValues = new Double[3];
        expectedValues[0] = 0.32435435;
        expectedValues[1] = 2.5;
        expectedValues[2] = ((double) 1) / 3;
        String[] valuesAsStrings = new String[expectedValues.length];
        for (int i = 0; i < expectedValues.length; i++) {
            valuesAsStrings[i] = expectedValues[i].toString();
        }

        Double[] transformations = new StringToDoubleTransformer().apply(valuesAsStrings);

        for (int i = 0; i < valuesAsStrings.length; i++) {
            assertEquals(expectedValues[i], transformations[i], "of " + i + "th element");
        }
    }

}
