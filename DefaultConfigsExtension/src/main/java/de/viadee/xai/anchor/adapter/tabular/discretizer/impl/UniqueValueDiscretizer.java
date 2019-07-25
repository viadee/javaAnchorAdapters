package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.CategoricalDiscretizationOrigin;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Discretizer mapping each value type to a unique integer value
 */
public class UniqueValueDiscretizer extends AbstractDiscretizer {
    private static final long serialVersionUID = -6185947730488220070L;

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values) {
        final Map<Serializable, Integer> valueToIndexDiscretizer = new HashMap<>();

        int index = 0;
        for (Serializable object : values) {
            final Integer previous = valueToIndexDiscretizer.putIfAbsent(object, index);
            if (previous == null) {
                index++;
            }
        }

        return valueToIndexDiscretizer.entrySet().stream().map(e ->
                new DiscretizationTransition(new CategoricalDiscretizationOrigin(e.getKey()), e.getValue().doubleValue()))
                .collect(Collectors.toList());
    }
}
