package de.viadee.xai.anchor.adapter.tabular.column;

import java.io.Serializable;
import java.util.List;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceNullTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

/**
 * Column used for transforming String inputs into number values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by number values
 */
public abstract class NumberColumn extends GenericColumn {
    private static final long serialVersionUID = -7652878190807257422L;

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public NumberColumn(String name) {
        this(name, null, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name         the column's name
     * @param dataTransformers the object value to replace null values with. Must be convertible to Integer values
     * @param anchorTransformers the transformations to apply before discretization for anchor
     * @param discretizer  the discretizer to use
     */
    public NumberColumn(String name, List<Transformer> dataTransformers, List<Transformer> anchorTransformers, Discretizer discretizer) {
        super(name, dataTransformers, anchorTransformers, discretizer);
    }

    /**
     * Create a default null transformer
     *
     * @param replaceNull the value to replace null with or null to create an exception in case of null
     * @return a matching {@link ReplaceNullTransformer}
     */
    static ReplaceNullTransformer createNullTransformer(final Serializable replaceNull) {
        return new ReplaceNullTransformer(
                (replaceNull == null) ? new ThrowExceptionOnNull() : replaceNull
        );
    }

    private static final class ThrowExceptionOnNull implements ReplaceNullTransformer.SerializableSupplier {
        private static final long serialVersionUID = -1290766005381209256L;

        @Override
        public Serializable get() {
            throw new IllegalArgumentException("This column has not been configured to handle null values.");
        }
    }
}
