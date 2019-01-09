package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Discretizer mapping each value type to a unique integer value
 */
public class UniqueValueDiscretizer implements Discretizer {
    private static final long serialVersionUID = -6185947730488220070L;

    private final Map<Serializable, Integer> valueToIndexDiscretizer = new HashMap<>();

    @Override
    public void fit(Serializable[] values) {
        int index = 0;
        for (Serializable object : values) {
            final Integer previous = valueToIndexDiscretizer.putIfAbsent(object, index);
            if (previous == null) {
                index++;
            }
        }
    }

    @Override
    public DiscretizerRelation unApply(int value) {
        Map.Entry<Serializable, Integer> disc = valueToIndexDiscretizer.entrySet().stream().filter((entry) -> entry.getValue().equals(value))
                .findFirst().orElseThrow(() -> new IllegalArgumentException("No value for discrete " + value + " found"));

        return new DiscretizerRelation(disc.getValue(), disc.getKey());
    }

    @Override
    public Integer apply(Serializable o) {
        final Integer result = valueToIndexDiscretizer.get(o);
        if (result == null)
            throw new IllegalArgumentException("Object did not appear during fitting");
        return result;
    }
}
