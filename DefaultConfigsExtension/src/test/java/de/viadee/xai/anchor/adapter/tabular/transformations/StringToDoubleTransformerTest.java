package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;

import org.junit.jupiter.api.Test;
import de.viadee.xai.anchor.adapter.tabular.transformations.StringToDoubleTransformer;

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

        Serializable[] transformations = new StringToDoubleTransformer().apply(valuesAsStrings);

        for (int i = 0; i < valuesAsStrings.length; i++) {
            assertEquals(expectedValues[i], transformations[i], "of " + i + "th element");
        }
    }

}
