package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.discretizers4j.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.AssertNumberTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceEmptyTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;
import de.viadee.xai.anchor.adapter.tabular.util.SerializableSupplier;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

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
        this(name, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name         the column's name
     * @param transformers the object value to replace null values with. Must be convertible to Integer values
     * @param discretizer  the discretizer to use
     */
    public NumberColumn(String name, List<Transformer> transformers, Discretizer discretizer) {
        super(name, transformers == null
                        ? Collections.singletonList(new AssertNumberTransformer())
                        : transformers,
                discretizer);
        if (transformers != null && transformers.stream().anyMatch(t -> t instanceof AssertNumberTransformer)) {
            addTransformer(new AssertNumberTransformer());
        }
    }

    /**
     * Create a default null transformer
     *
     * @param replaceNull the value to replace null with or null to create an exception in case of null
     * @return a matching {@link ReplaceEmptyTransformer}
     */
    static ReplaceEmptyTransformer createEmptyTransformator(final Serializable replaceNull) {
        return new ReplaceEmptyTransformer(
                (replaceNull == null) ? new ThrowExceptionOnNull() : replaceNull
        );
    }

    private static final class ThrowExceptionOnNull implements SerializableSupplier {
        private static final long serialVersionUID = -1290766005381209256L;

        @Override
        public Serializable get() {
            throw new IllegalArgumentException("This column has not been configured to handle null values.");
        }
    }
}
