package de.viadee.xai.anchor.adapter.tabular;

import java.io.Serializable;

import org.junit.jupiter.api.Test;
import de.viadee.xai.anchor.adapter.tabular.transformations.StringToIntTransformer;

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

        Serializable[] transformations = new StringToIntTransformer().apply(valuesAsStrings);

        for (int i = 0; i < valuesAsStrings.length; i++) {
            assertEquals(expectedValues[i], transformations[i], "of " + i + "th element");
        }
    }

}
