package de.viadee.anchorj.tabular.discretizer;

import java.util.HashMap;
import java.util.Map;

/**
 * Discretizer mapping each value to its integer value
 */
public class UniqueValueDiscretizer extends AbstractDiscretizer<Integer> {

    private final Map<Object, Integer> valueToIndexDiscretizer = new HashMap<>();

    @Override
    public void fit(Object[] values) {
        int index = 0;
        for (Object object : values) {
            final Integer previous = valueToIndexDiscretizer.putIfAbsent(object, index);
            if (previous == null)
                index++;
        }
    }

    @Override
    public boolean isResultNumeric() {
        return true;
    }

    @Override
    public Integer apply(Object o) {
        final Integer result = valueToIndexDiscretizer.get(o);
        if (result == null)
            throw new IllegalArgumentException("Object did not appear during fitting");
        return result;
    }
}
