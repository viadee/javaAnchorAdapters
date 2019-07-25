package de.viadee.xai.anchor.adapter.tabular.discretizer;

import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.NumericDiscretizationOrigin;
import org.junit.jupiter.api.Test;

import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ManualDiscretizerTest {

    private static int getTransitionSize(ManualDiscretizer manualDiscretizer) {
        Field discretizationTransitions = null;
        try {
            discretizationTransitions = AbstractDiscretizer.class.getDeclaredField("discretizationTransitions");
            discretizationTransitions.setAccessible(true);
            return ((Collection) discretizationTransitions.get(manualDiscretizer)).size();
        } catch (NoSuchFieldException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void testDiscretization() {
        ManualDiscretizer manualDiscretizer = new ManualDiscretizer(20, 40, 60, 80);
        manualDiscretizer.fit(new Serializable[]{50});

        assertEquals(4, getTransitionSize(manualDiscretizer));

        // Adding a class from min to boundary is expected behavior
        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(20D).getDiscretizationOrigin()).getMinValue());
        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(20D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMinValue());
        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMinValue());
        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMinValue());
        assertEquals(80D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMaxValue());
    }

    @Test
    public void testDiscretizationAdaptedLowerClass() {
        ManualDiscretizer manualDiscretizer = new ManualDiscretizer(20, 40, 60, 80);
        manualDiscretizer.fit(new Serializable[]{0});

        assertEquals(4, getTransitionSize(manualDiscretizer));

        assertEquals(0D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(10D).getDiscretizationOrigin()).getMinValue());
        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(10D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMinValue());
        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMinValue());
        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMinValue());
        assertEquals(80D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMaxValue());
    }

    @Test
    public void testDiscretizationNewUpperClass() {
        ManualDiscretizer manualDiscretizer = new ManualDiscretizer(20, 40, 60, 80);
        manualDiscretizer.fit(new Serializable[]{110});

        assertEquals(5, getTransitionSize(manualDiscretizer));

        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(20D).getDiscretizationOrigin()).getMinValue());
        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(20D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMinValue());
        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMinValue());
        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMinValue());
        assertEquals(80D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(80D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(95D).getDiscretizationOrigin()).getMinValue());
        assertEquals(110D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(95D).getDiscretizationOrigin()).getMaxValue());
    }

    @Test
    public void testDiscretizationAdapterLowerAndNewUpperClass() {
        ManualDiscretizer manualDiscretizer = new ManualDiscretizer(20, 40, 60, 80);
        manualDiscretizer.fit(new Serializable[]{10, 110});

        assertEquals(5, getTransitionSize(manualDiscretizer));

        assertEquals(10D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(15D).getDiscretizationOrigin()).getMinValue());
        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(15D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(20D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMinValue());
        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(30D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(40D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMinValue());
        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(50D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(60D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMinValue());
        assertEquals(80D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(70D).getDiscretizationOrigin()).getMaxValue());

        assertEquals(80D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(95D).getDiscretizationOrigin()).getMinValue());
        assertEquals(110D, ((NumericDiscretizationOrigin) manualDiscretizer
                .getTransition(95D).getDiscretizationOrigin()).getMaxValue());
    }

}