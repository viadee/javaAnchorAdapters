package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.function.Function;

/**
 * Represents functionality every column needs to provide: discretization.
 *
 * Discretization is used by Anchors tabular to perturb instances and find similar neighbours.
 *
 */
@SuppressWarnings("unused")
public interface Discretizer extends Function<Serializable, Integer>, Serializable {

    /**
     * Fits the discretizer and passes all values that it might get asked to discretize
     *
     * @param values the domain
     */
    void fit(Serializable[] values);

    DiscretizerRelation unFit(int value);
}
