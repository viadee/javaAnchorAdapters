package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Discretizer allowing to specify custom classes.
 * <p>
 * The new target of these classes will be its average value
 * <p>
 * The lower class's lower boundary might be adapted during fit to include the minimum value
 * <p>
 * A new upper class might be created during fit to include the max values
 */
public class ManualDiscretizer extends AbstractDiscretizer {
    private final SortedSet<Double> classBoundaries;

    /**
     * Creates the discretizer by specifying upper class boundaries
     *
     * @param classBoundaries the upper class boundaries to be used
     */
    public ManualDiscretizer(Double... classBoundaries) {
        super(false);
        this.classBoundaries = new TreeSet<>(Arrays.asList(classBoundaries));
    }

    /**
     * Creates the discretizer by specifying upper class boundaries
     *
     * @param classBoundaries the upper class boundaries to be used
     */
    public ManualDiscretizer(Integer... classBoundaries) {
        super(false);
        this.classBoundaries = Arrays.stream(classBoundaries).map(Integer::doubleValue)
                .collect(Collectors.toCollection(TreeSet::new));
    }

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels) {
        if (Stream.of(values).anyMatch(v -> !(v instanceof Number))) {
            throw new IllegalArgumentException("Only numeric values allowed for this discretizer");
        }
        final List<DiscretizationTransition> result = new ArrayList<>();

        final List<Double> vals = Stream.of(values).map(v -> ((Number) v).doubleValue()).collect(Collectors.toList());

        final Double min = Math.min(Collections.min(vals), Collections.min(classBoundaries));
        final Double max = Math.max(Collections.max(vals), Collections.max(classBoundaries));

        Double currentLowerBoundary = min;
        for (Double boundary : classBoundaries) {
            Double average = (currentLowerBoundary + boundary) / 2;
            result.add(new DiscretizationTransition(
                    new NumericDiscretizationOrigin(currentLowerBoundary, boundary),
                    average));
            currentLowerBoundary = boundary;
        }
        if (result.isEmpty()) {
            throw new IllegalArgumentException("Discretizer could not be fit as no classes could be derived from " +
                    "passed boundary parameters");
        }
        // If max is higher then last set boundary add one more
        if (!((NumericDiscretizationOrigin) result.get(result.size() - 1).getDiscretizationOrigin()).getMaxValue()
                .equals(max)) {
            result.add(new DiscretizationTransition(
                    new NumericDiscretizationOrigin(currentLowerBoundary, max),
                    (currentLowerBoundary + max) / 2));
        }

        return result;
    }
}
