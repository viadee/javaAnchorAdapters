package de.viadee.xai.anchor.adapter.tabular.discretizer;

import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

class FUSINTERDiscretizerTest {

    /**
     * Tests Step 1 (Sorting) of FUSINTER Algorithm
     */
    @Test
    void testSortingOfArray() {
        FUSINTERDiscretizer fusinterDiscretizer = new FUSINTERDiscretizer();
        List<FUSINTERDiscretizer.Interval> list = fusinterDiscretizer.fitCreateSupervisedTransitions(
                new Number[][]{
                        {1.0, 0},
                        {0.0, 0},
                        {2.0, 0},
                        {1.5, 1},
                        {1.6, 1}
                }
        );
        assertEquals(3, list.size());
    }

    /**
     * Tests Step 2 (Initial Splitting) of FUSINTER Algorithm
     */
    @Test
    void testEqualClassSplittingSeparateClasses() {
        FUSINTERDiscretizer fusinterDiscretizer = new FUSINTERDiscretizer();
        List<FUSINTERDiscretizer.Interval> list = fusinterDiscretizer.equalClassSplit(
                new Number[][]{
                        {0.0, 0},
                        {1.0, 0},
                        {2.0, 0},
                        {3.0, 1},
                        {4.0, 1},
                        {5.0, 1},
                        {6.0, 1},
                        {7.0, 0},
                        {8.0, 0},
                        {9.0, 0},
                        {10.0, 0},
                }
        );
        assertEquals(3, list.size());
    }

    /**
     * Tests Step 3 (Values with Mixed Classes Have own Interval) of FUSINTER Algorithm
     */
    @Test
    void testEqualClassSplittingMixedClasses() {
        FUSINTERDiscretizer fusinterDiscretizer = new FUSINTERDiscretizer();
        List<FUSINTERDiscretizer.Interval> list = fusinterDiscretizer.equalClassSplit(
                new Number[][]{
                        {0.0, 0},
                        {1.0, 0},
                        {2.0, 0},
                        {3.0, 1},
                        {4.0, 1},
                        {5.0, 1},
                        {5.0, 0},
                        {5.0, 0},
                        {6.0, 1},
                        {7.0, 0},
                        {8.0, 0},
                        {9.0, 0},
                        {10.0, 0},
                }
        );
        assertEquals(5, list.size());
    }

    @Test
    void testIntervalDistribution() {
        Number[][] values = new Number[][]{
                {0.0, 0},
                {1.0, 0},
                {2.0, 0},
                {3.0, 1},
                {4.0, 1},
                {5.0, 1},
                {5.0, 0},
                {5.0, 0},
                {6.0, 1},
                {7.0, 0},
                {8.0, 0},
                {9.0, 0},
                {10.0, 0},
        };
        int[] stream = IntStream.range(0, values.length)
                .map(i -> values[i][0].intValue()).distinct().toArray();
        for (int i = 0; i < stream.length; i++) {
            System.out.println(stream[i]);
        }
    }

}