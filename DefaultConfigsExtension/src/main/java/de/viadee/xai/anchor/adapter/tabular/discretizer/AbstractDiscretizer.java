package de.viadee.xai.anchor.adapter.tabular.discretizer;

import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.NumericDiscretizationOrigin;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * Abstract discretizer managing both the discretization and un-discretization for all discretizers that choose to work
 * with the {@link DiscretizationTransition}
 */
public abstract class AbstractDiscretizer implements Discretizer {
    private List<DiscretizationTransition> discretizationTransitions;

    @Override
    public Collection<DiscretizationTransition> getTransitions() {
        return discretizationTransitions;
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
            // This could be required for some scenarios.. Not yet, though
            throw new IllegalArgumentException("Discretization targets are ambiguous");
        }

        // In case all origins are numeric we need to set open lower and upper boundaries
        if (this.discretizationTransitions.size() > 1 && this.discretizationTransitions.stream()
                .allMatch(d -> d.getDiscretizationOrigin() instanceof NumericDiscretizationOrigin)) {
            DiscretizationTransition minDisc = this.discretizationTransitions.get(0);
            DiscretizationTransition maxDisc = this.discretizationTransitions.get(0);
            for (final DiscretizationTransition current : this.discretizationTransitions) {
                if (((NumericDiscretizationOrigin) current.getDiscretizationOrigin()).getMinValue().doubleValue() <
                        ((NumericDiscretizationOrigin) minDisc.getDiscretizationOrigin()).getMinValue().doubleValue())
                    minDisc = current;
                if (((NumericDiscretizationOrigin) current.getDiscretizationOrigin()).getMaxValue().doubleValue() >
                        ((NumericDiscretizationOrigin) maxDisc.getDiscretizationOrigin()).getMaxValue().doubleValue())
                    maxDisc = current;
            }
            ((NumericDiscretizationOrigin) minDisc.getDiscretizationOrigin()).setFirst(true);
            ((NumericDiscretizationOrigin) maxDisc.getDiscretizationOrigin()).setLast(true);
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
    public Double apply(Serializable serializable) {
        final DiscretizationTransition discretizationTransition = discretizationTransitions.stream()
                .filter(d -> d.getDiscretizationOrigin().canDiscretize(serializable))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Could not find transition for " + serializable));
        return discretizationTransition.getDiscretizedValue();
    }
}
