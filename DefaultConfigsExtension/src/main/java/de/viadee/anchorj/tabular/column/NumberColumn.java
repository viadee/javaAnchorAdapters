package de.viadee.anchorj.tabular.column;

import de.viadee.anchorj.tabular.discretizer.Discretizer;
import de.viadee.anchorj.tabular.transformations.ReplaceNullTransformer;
import de.viadee.anchorj.tabular.transformations.Transformer;

import java.util.List;
import java.util.function.Supplier;

/**
 * Column used for transforming String inputs into number values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by number values
 */
abstract class NumberColumn extends GenericColumn {

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    NumberColumn(String name) {
        this(name, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name         the column's name
     * @param transformers the object value to replace null values with. Must be convertable to Integer values
     * @param discretizer  the discretizer to use
     */
    NumberColumn(String name, List<Transformer> transformers, Discretizer discretizer) {
        super(name, transformers, discretizer);
    }

    /**
     * Create a default null transformer
     *
     * @param replaceNull the value to replace null with or null to create an exception in case of null
     * @return a matching {@link ReplaceNullTransformer}
     */
    static ReplaceNullTransformer createNullTransformer(final Object replaceNull) {
        return new ReplaceNullTransformer((replaceNull == null)
                ? new ThrowExceptionOnNull()
                : replaceNull);
    }


    static final class ThrowExceptionOnNull implements Supplier<Object> {
        @Override
        public Object get() {
            throw new IllegalArgumentException("This column has not been configured to handle null values.");
        }
    }
}
