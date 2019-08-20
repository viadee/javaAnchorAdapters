package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;
import org.junit.jupiter.api.Test;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PercentileMedianDiscretizerTest {

    @Test
    public void testErrorThrownOnNonNumbers() {

        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3, false);
        assertThrows(IllegalArgumentException.class, () ->
                percentileMedianDiscretizer.fit(new Serializable[]{1, 2, "thisshouldbreakeverything"}));
        assertThrows(IllegalArgumentException.class, () ->
                percentileMedianDiscretizer.fit(new Serializable[]{"thisshouldbreakeverything", "thiseither"}));

    }

    @Test
    public void testFittingAllowedOnceOnly() {
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3, false);
        percentileMedianDiscretizer.fit(new Integer[]{1, 2, 3});
        assertThrows(IllegalArgumentException.class, () ->
                percentileMedianDiscretizer.fit(new Integer[]{1, 2, 3}));
    }

    @Test
    public void testDiscretizationOdd() {
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3, false);
        percentileMedianDiscretizer.fit(new Double[]{1D, 2D, 3D, 0.2, 1.3, 2.4});
        Double[] discretization = percentileMedianDiscretizer.apply(new Double[]{1D, 1.9, 2.9});
        assertArrayEquals(new Double[]{0.6, 1.65, 2.7}, discretization);
    }

    @Test
    public void testDiscretizationEven() {
        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3, false);
        percentileMedianDiscretizer.fit(IntStream.range(1, 21).boxed().toArray(Integer[]::new));
        Double[] discretization = percentileMedianDiscretizer.apply(new Double[]{4D, 2D, 10D, 20D});
        assertArrayEquals(new Double[]{4D, 4D, 11D, 17.5}, discretization);
    }

    @Test
    public void testClassReduction() {
        PercentileMedianDiscretizer failingDiscretizer = new PercentileMedianDiscretizer(2, false);
        assertThrows(IllegalArgumentException.class, () ->
                failingDiscretizer.fit(new Integer[]{1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3}));

        PercentileMedianDiscretizer percentileMedianDiscretizer = new PercentileMedianDiscretizer(3, true);
        percentileMedianDiscretizer.fit(new Integer[]{0, 0, 0, 0, 0, 0, 0, 0, 5, 10, 15});

        Double[] discretization = percentileMedianDiscretizer.apply(new Double[]{0D, 5D, 10D, 15D});
        assertArrayEquals(new Double[]{0D, 10D, 10D, 10D}, discretization);
    }

    @Test
    void testMaxIsMaxOfOthersMerge() {
        PercentileMedianDiscretizer discretizer = new PercentileMedianDiscretizer(3, true);
        discretizer.fit(new Integer[]{1, 2, 3, 4, 5, 5, 5, 5, 5, 5, 6, 7, 8, 9, 10});

        List<DiscretizationTransition> list = new ArrayList<>(discretizer.getTransitions());
        Double[] doubles = discretizer.apply(new Integer[]{5});
        assertArrayEquals(new Double[]{3D}, doubles);

    }

}