package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

/**
 * Abstract discretizer managing both the discretization and un-discretization for all discretizers that choose to work
 * with the {@link DiscretizationTransition}
 */
public abstract class AbstractDiscretizer implements Discretizer {
    private List<DiscretizationTransition> discretizationTransitions;

    @Override
    public DiscretizationTransition getTransition(int discretizedValue) {
        return discretizationTransitions.stream().filter(d -> d.getDiscretizedValue() == discretizedValue).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Could not find transition for discretized value" +
                        discretizedValue));
    }

    @Override
    public void fit(Serializable[] values) {
        if (values == null || values.length == 0) {
            // all values are single class values
            throw new IllegalArgumentException("No values for fitting procedure passed");
        }


        if (discretizationTransitions != null) {
            throw new IllegalArgumentException("Discretizer has already been fitted");
        }

        this.discretizationTransitions = fitCreateTransitions(values);

        final long distinctDiscretizedValues = discretizationTransitions.stream()
                .map(DiscretizationTransition::getDiscretizedValue).distinct().count();
        if (distinctDiscretizedValues != discretizationTransitions.size()) {
            this.discretizationTransitions = null;
            // TODO should we allow this?
            throw new IllegalArgumentException("Discretization targets are ambiguous");
        }
    }

    /**
     * Fits on the data
     *
     * @param values the values to be fitted on
     * @return a {@link java.util.Collection} containing the {@link DiscretizationTransition}s
     */
    protected abstract List<DiscretizationTransition> fitCreateTransitions(Serializable[] values);

    @Override
    public Integer apply(Serializable serializable) {
        final DiscretizationTransition discretizationTransition = discretizationTransitions.stream()
                .filter(d -> d.getDiscretizationOrigin().canDiscretize(serializable))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Could not find transition for " + serializable));
        return discretizationTransition.getDiscretizedValue();
    }
}
