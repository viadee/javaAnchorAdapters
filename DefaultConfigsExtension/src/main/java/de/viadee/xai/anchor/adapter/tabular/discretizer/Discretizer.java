package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.function.Function;

/**
 * Represents functionality every column needs to provide: discretization.
 * <p>
 * Discretization is used by Anchors tabular to perturb instances and find similar neighbours.
 */
public interface Discretizer extends Function<Serializable, Integer>, Serializable {

    /**
     * Applies this discretizer to the passed data
     *
     * @param data the data to discretize
     * @return the discretized data
     */
    default Integer[] apply(Serializable[] data) {
        Integer[] discretizedData = new Integer[data.length];
        for (int i = 0; i < data.length; i++) {
            discretizedData[i] = this.apply(data[i]);
        }

        return discretizedData;
    }

    /**
     * Fits the discretizer and passes all values that it might get asked to discretize
     *
     * @param values the domain
     */
    void fit(Serializable[] values);


    /**
     * This method returns the relation for a certain discretized value.
     * <p>
     * This allows to unApply a discretization and obtain the original value
     *
     * @param discretizedValue the value to get the relation for
     * @return the {@link DiscretizationTransition}
     */
    DiscretizationTransition getTransition(int discretizedValue);
}
