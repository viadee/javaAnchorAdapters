package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.function.Function;

import de.viadee.anchorj.tabular.column.GenericColumn;

/**
 * Represents functionality every column needs to provide: discretization.
 * <p>
 * Discretization is used by Anchors tabular to perturb instances and find similar neighbours.
 */
public interface Discretizer extends Function<Serializable, Integer>, Serializable {

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
    void fit(GenericColumn column, Serializable[] values);

    DiscretizerRelation unApply(int value);
}
