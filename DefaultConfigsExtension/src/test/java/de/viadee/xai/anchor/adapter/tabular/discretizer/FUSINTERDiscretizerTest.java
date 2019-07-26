package de.viadee.xai.anchor.adapter.tabular.discretizer;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
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
                        {3.0, 0},
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
    void testEqualClassSplittingSeparateClasses() throws Exception {
        FUSINTERDiscretizer fusinterDiscretizer = new FUSINTERDiscretizer();
        Field targetValues = FUSINTERDiscretizer.class.getDeclaredField("targetValues");
        targetValues.setAccessible(true);
        targetValues.set(fusinterDiscretizer, new int[]{0, 1});
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
    void testEqualClassSplittingMixedClasses() throws Exception {
        FUSINTERDiscretizer fusinterDiscretizer = new FUSINTERDiscretizer();
        Field targetValues = FUSINTERDiscretizer.class.getDeclaredField("targetValues");
        targetValues.setAccessible(true);
        targetValues.set(fusinterDiscretizer, new int[]{0, 1});
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
    void testIntervalReduction() {
        FUSINTERDiscretizer fusinterDiscretizer = new FUSINTERDiscretizer();
        List<FUSINTERDiscretizer.Interval> list = fusinterDiscretizer.fitCreateSupervisedTransitions(
                new Number[][]{
                        {1.0, 0},
                        {2.0, 1},
                        {3.0, 1},
                        {4.0, 1},
                        {5.0, 1},
                        {6.0, 1},
                        {7.0, 1},
                        {8.0, 1},
                        {9.0, 1},
                        {10.0, 1},
                        {11.0, 1},
                        {12.0, 1},
                        {13.0, 1},
                        {14.0, 1},
                        {15.0, 1},
                        {16.0, 1},
                        {17.0, 1},
                        {18.0, 1},
                        {19.0, 1},
                        {20.0, 0},
                }
        );
        assertEquals(1, list.size());
    }

    @Test
    void testIntervalReductionBig() {
        FUSINTERDiscretizer fusinterDiscretizer = new FUSINTERDiscretizer();
        List<FUSINTERDiscretizer.Interval> list = fusinterDiscretizer.fitCreateSupervisedTransitions(
                new Number[][]{
                        {1.0, 1},
                        {2.0, 0},
                        {3.0, 1},
                        {3.0, 1},
                        {4.0, 1},
                        {4.0, 1},
                        {5.0, 1},
                        {5.0, 1},
                        {5.0, 1},
                        {6.0, 1},
                        {6.0, 1},
                        {6.0, 1},
                        {7.0, 1},
                        {7.0, 1},
                        {7.0, 1},
                        {8.0, 1},
                        {8.0, 1},
                        {9.0, 1},
                        {9.0, 1},
                        {9.0, 1},
                        {10.0, 1},
                        {10.0, 1},
                        {10.0, 1},
                        {11.0, 1},
                        {11.0, 1},
                        {11.0, 1},
                        {12.0, 1},
                        {12.0, 1},
                        {13.0, 1},
                        {13.0, 1},
                        {13.0, 0},
                        {14.0, 1},
                        {14.0, 1},
                        {14.0, 1},
                        {15.0, 0},
                        {15.0, 0},
                        {15.0, 0},
                        {16.0, 1},
                        {16.0, 1},
                        {16.0, 1},
                        {17.0, 0},
                        {17.0, 0},
                        {17.0, 0},
                        {18.0, 1},
                        {18.0, 1},
                        {18.0, 0},
                        {19.0, 0},
                        {19.0, 0},
                        {20.0, 1},
                        {20.0, 1},
                        {22.0, 1},
                        {22.0, 1},
                        {22.0, 1},
                        {23.0, 0},
                        {23.0, 0},
                        {23.0, 0},
                        {27.0, 0},
                        {27.0, 0},
                        {28.0, 0},
                        {28.0, 0},
                        {28.0, 0},
                        {29.0, 0},
                        {29.0, 0},
                        {29.0, 0},
                        {30.0, 0},
                        {30.0, 0},
                        {30.0, 0},
                        {31.0, 0},
                        {31.0, 0},
                        {31.0, 0},
                        {33.0, 0},
                        {34.0, 0},
                        {34.0, 0},
                        {34.0, 0},
                        {35.0, 0},
                        {35.0, 0},
                        {35.0, 0},
                        {36.0, 0},
                        {36.0, 0},
                        {36.0, 0},
                        {37.0, 0},
                        {37.0, 0},
                        {37.0, 1},
                        {38.0, 1},
                        {38.0, 1},
                        {38.0, 0},
                        {39.0, 0},
                        {39.0, 0},
                        {40.0, 1},
                        {40.0, 1},
                }
        );

        assertEquals(0, list.get(0).getBegin());
        assertEquals(33, list.get(0).getEnd());
        assertEquals(34, list.get(1).getBegin());
        assertEquals(47, list.get(1).getEnd());
        assertEquals(48, list.get(2).getBegin());
        assertEquals(52, list.get(2).getEnd());
        assertEquals(53, list.get(3).getBegin());
        assertEquals(79, list.get(3).getEnd());
        assertEquals(80, list.get(4).getBegin());
        assertEquals(89, list.get(4).getEnd());
    }

}