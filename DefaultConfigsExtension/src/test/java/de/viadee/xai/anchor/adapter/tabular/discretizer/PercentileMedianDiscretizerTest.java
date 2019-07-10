package de.viadee.xai.anchor.adapter.tabular.discretizer;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.Serializable;

import static org.junit.jupiter.api.Assertions.*;

class PercentileMedianDiscretizerTest {

    @Test
    void test0to1Distribution() {

        // Given
        Serializable[] values = {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.1,0.2,0.3,1,1,1,1,1,1,1};
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3,-1);

        // Then
        percentileMedianDiscretizer.fit(values);
    }

    @Test
    void testRemovalOfRelationsWithSameMedian() {

        // Given
        Serializable[] values = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,4,4,4,8,8,8,8};
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3,-1);

        // Then
        percentileMedianDiscretizer.fit(values);
    }

    @Test
    void testEmptyValues() {

        // Given
        Serializable[] values = {};
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3,-1);

        // Then
        Assertions.assertThrows(RuntimeException.class, () -> percentileMedianDiscretizer.fit(values), "Expected .fit() to throw RuntimeException but it didn't.");
    }
}