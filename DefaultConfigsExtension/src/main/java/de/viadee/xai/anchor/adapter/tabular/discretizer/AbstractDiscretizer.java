package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.List;

/**
 * Abstract discretizer managing both the discretization and un-discretization for all discretizers that choose to work
 * with the {@link DiscretizationTransition}
 */
public abstract class AbstractDiscretizer implements Discretizer {
    private final boolean isSupervised;

    private List<DiscretizationTransition> discretizationTransitions;

    /**
     * Constructs the instance
     *
     * @param isSupervised true, if this is a supervised discretizer
     */
    AbstractDiscretizer(boolean isSupervised) {
        this.isSupervised = isSupervised;
    }

    @Override
    public DiscretizationTransition getTransition(Double discretizedValue) {
        return discretizationTransitions.stream().filter(d -> discretizedValue.equals(d.getDiscretizedValue())).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Could not find transition for discretized value" +
                        discretizedValue));
    }

    /**
     * Fits the discretizer and passes all values that it might get asked to discretize
     *
     * @param values the domain
     */
    public void fit(Serializable[] values) {
        this.fit(values, null);
    }

    @Override
    public void fit(Serializable[] values, Double[] labels) {
        if (values == null || values.length == 0) {
            // all values are single class values
            throw new IllegalArgumentException("No values for fitting procedure passed");
        }

        if (isSupervised && labels == null) {
            throw new IllegalArgumentException("Labels need to be specified for supervised discretizers");
        }

        if (isSupervised && labels.length != values.length) {
            throw new IllegalArgumentException("Labels need to be of same length as column values");
        }

        if (discretizationTransitions != null) {
            throw new IllegalArgumentException("Discretizer has already been fitted");
        }

        this.discretizationTransitions = fitCreateTransitions(values, labels);

        final long distinctDiscretizedValues = discretizationTransitions.stream()
                .map(DiscretizationTransition::getDiscretizedValue).distinct().count();
        if (distinctDiscretizedValues != discretizationTransitions.size()) {
            this.discretizationTransitions = null;
            // This could be required for some scenarios.. Not yet, though
            throw new IllegalArgumentException("Discretization targets are ambiguous");
        }
    }

    /**
     * Fits on the data
     *
     * @param values the values to be fitted on
     * @param labels the labels. != null, iff supervised
     * @return a {@link java.util.Collection} containing the {@link DiscretizationTransition}s
     */
    protected abstract List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels);

    @Override
    public Double apply(Serializable serializable) {
        final DiscretizationTransition discretizationTransition = discretizationTransitions.stream()
                .filter(d -> d.getDiscretizationOrigin().canDiscretize(serializable))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Could not find transition for " + serializable));
        return discretizationTransition.getDiscretizedValue();
    }
}
