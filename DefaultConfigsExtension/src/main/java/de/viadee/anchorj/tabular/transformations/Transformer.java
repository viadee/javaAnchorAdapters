package de.viadee.anchorj.tabular.transformations;

import java.io.Serializable;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Represents a transformator used to transform read values to another state
 */
public interface Transformer extends Function<Serializable, Serializable>, Serializable {
    default Serializable[] apply(Serializable[] values) {
        return Stream.of(values).map(this).toArray(Serializable[]::new);
    }
}
