package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.transformations.StringToBooleanTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.util.Arrays;
import java.util.List;

/**
 * Column used for transforming String inputs into boolean values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by integer values
 */
public class BooleanColumn extends NumberColumn {

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public BooleanColumn(String name) {
        this(name, null);
    }

    /**
     * Instantiates the column
     *
     * @param name               the column's name
     * @param transformers   the object value to replace null values with. Must be convertible to double values
     */
    public BooleanColumn(String name, List<Transformer> transformers) {
        super(name, transformers, null);
    }

    /**
     * Instantiates the column
     *
     * @param name the column's name
     * @return the corresponding column object
     */
    public static BooleanColumn fromStringInput(String name) {
        return new BooleanColumn(
                name,
                Arrays.asList(createEmptyTransformator(-1), new StringToBooleanTransformer()));
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param replaceNull the object value to replace null values with. Must be convertible to double values
     * @return the corresponding column object
     */
    public static BooleanColumn fromStringInput(String name, Integer replaceNull) {
        return new BooleanColumn(
                name,
                Arrays.asList(createEmptyTransformator(replaceNull), new StringToBooleanTransformer()));
    }
}
