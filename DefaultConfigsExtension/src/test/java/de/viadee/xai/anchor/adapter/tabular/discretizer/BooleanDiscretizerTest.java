package de.viadee.xai.anchor.adapter.tabular.discretizer;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.Serializable;

class BooleanDiscretizerTest {

    @Test
    void testUnApply0() {
        // Given
        int value = 0;
        BooleanDiscretizer booleanDiscretizer = new BooleanDiscretizer();

        // When
        DiscretizerRelation relation = booleanDiscretizer.unApply(value);

        // Then
        Assertions.assertEquals(Boolean.FALSE, relation.getCategoricalValue(), "0 should be representative for false");
    }

    @Test
    void testUnApply1() {
        // Given
        int value = 1;
        BooleanDiscretizer booleanDiscretizer = new BooleanDiscretizer();

        // When
        DiscretizerRelation relation = booleanDiscretizer.unApply(value);

        // Then
        Assertions.assertEquals(Boolean.TRUE, relation.getCategoricalValue(), "1 should be representative for true");
    }

    @Test
    void testApplyTrue() {
        // Given
        Serializable s = "TRUE";
        BooleanDiscretizer booleanDiscretizer = new BooleanDiscretizer();

        // When
        int result = booleanDiscretizer.apply(s);

        // Then
        Assertions.assertEquals(1, result, "serializable TRUE should be 1");
    }

    @Test
    void testApplyFalse() {
        // Given
        Serializable s = "FALSE";
        BooleanDiscretizer booleanDiscretizer = new BooleanDiscretizer();

        // When
        int result = booleanDiscretizer.apply(s);

        // Then
        Assertions.assertEquals(0, result, "serializable TRUE should be 0");
    }

    @Test
    void testApplyTrueLowerCase() {
        // Given
        Serializable s = "true";
        BooleanDiscretizer booleanDiscretizer = new BooleanDiscretizer();

        // When
        int result = booleanDiscretizer.apply(s);

        // Then
        Assertions.assertEquals(1, result, "serializable TRUE should be 1");
    }

    @Test
    void testApplyNoBool() {
        // Given
        Serializable s = "tr";
        BooleanDiscretizer booleanDiscretizer = new BooleanDiscretizer();

        // When

        // Then
        Assertions.assertThrows(RuntimeException.class, () -> booleanDiscretizer.apply(s));
    }

    @Test
    void testApplyNumber() {
        // Given
        Serializable s = "1";
        BooleanDiscretizer booleanDiscretizer = new BooleanDiscretizer();

        // When

        // Then
        Assertions.assertThrows(RuntimeException.class, () -> booleanDiscretizer.apply(s));
    }


}