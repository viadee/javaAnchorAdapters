package de.viadee.xai.anchor.adapter.tabular.discretizer;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.Serializable;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class PercentileMedianDiscretizerTest {

    @Test
    void test0to1Distribution() {
        // Given
        Serializable[] values = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.2, 0.3, 1, 1, 1, 1, 1, 1, 1};
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(5, -1);

        // When
        percentileMedianDiscretizer.fit(values);
        List<DiscretizerRelation> list = percentileMedianDiscretizer.getDiscretizerRelations();

        // Then
        // Before the distribution between 0 and 1 always had only 2 classes being 0 and 1 because of the cast from double to int of the discretization value
        Assertions.assertTrue(list.size() > 2);
    }

    @Test
    void testRemovalOfRelationsWithSameMedian() {
        // Given
        Serializable[] values = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4, 4, 4, 8, 8, 8, 8};
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3, -1);

        // When
        percentileMedianDiscretizer.fit(values);
        List<DiscretizerRelation> list = percentileMedianDiscretizer.getDiscretizerRelations();
        boolean result = true;

        for (DiscretizerRelation r : list) {
            for (DiscretizerRelation r2 : list) {
                if (r.getConditionMin().equals(r2.getConditionMax()) && r != r2)
                    result = false;
            }
        }

        // Then
        Assertions.assertTrue(result);
    }

    @Test
    void testEmptyValues() {
        // Given
        Serializable[] values = {};
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3, -1);

        // Then
        Assertions.assertThrows(RuntimeException.class, () -> percentileMedianDiscretizer.fit(values), "Expected .fit() to throw RuntimeException but it didn't.");
    }
}