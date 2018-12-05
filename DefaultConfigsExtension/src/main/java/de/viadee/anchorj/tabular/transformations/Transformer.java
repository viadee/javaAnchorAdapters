package de.viadee.anchorj.tabular.transformations;

import java.util.function.Function;

/**
 * Represents a transformator used to transform read values to another state
 */
public interface Transformer extends Function<Object[], Object[]> {
}
