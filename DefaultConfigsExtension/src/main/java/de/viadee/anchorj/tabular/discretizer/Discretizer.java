package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.function.Function;

/**
 * Represents functionality every column needs to provide: discretization.
 *
 * Discretization is used by Anchors tabular to perturb instances and find similar neighbours.
 *
 */
public interface Discretizer extends Function<Object, Integer>, Serializable {

    /**
     * Fits the discretizer and passes all values that it might get asked to discretize
     *
     * @param values the domain
     */
    void fit(Object[] values);
}
