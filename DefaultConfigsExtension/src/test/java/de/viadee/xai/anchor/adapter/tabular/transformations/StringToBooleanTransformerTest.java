package de.viadee.xai.anchor.adapter.tabular.transformations;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class StringToBooleanTransformerTest {

    @Test
    void testApplyTrue() {
        //Given
        String test = "TRUE";
        StringToBooleanTransformer stringToBooleanTransformer = new StringToBooleanTransformer();

        //When
        boolean result = stringToBooleanTransformer.apply(test);

        //Then
        assertEquals(Boolean.TRUE, result);
    }

    @Test
    void testApplyFalse() {
        //Given
        String test = "False";
        StringToBooleanTransformer stringToBooleanTransformer = new StringToBooleanTransformer();

        //When
        boolean result = stringToBooleanTransformer.apply(test);

        //Then
        assertEquals(Boolean.FALSE, result);
    }

    @Test
    void testApplyNoBool() {
        //Given
        String test = "test";
        StringToBooleanTransformer stringToBooleanTransformer = new StringToBooleanTransformer();

        //Then
        Assertions.assertThrows(IllegalArgumentException.class, () -> stringToBooleanTransformer.apply(test));
    }

    @Test
    void testApplyInt() {
        //Given
        String test = "1";
        StringToBooleanTransformer stringToBooleanTransformer = new StringToBooleanTransformer();

        //Then
        Assertions.assertThrows(IllegalArgumentException.class, () -> stringToBooleanTransformer.apply(test));
    }
}