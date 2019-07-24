package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.StringToBooleanTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.util.ArrayList;
import java.util.List;

/**
 * Column used for transforming String inputs into boolean values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by integer values
 */
public class BooleanColumn extends GenericColumn {

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
     * @param name         the column's name
     * @param transformers the object value to replace null values with. Must be convertible to double values
     */
    public BooleanColumn(String name, List<Transformer> transformers) {
        super(name, transformers, new UniqueValueDiscretizer());
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
                addStringTransformer(null));
    }

    private static List<Transformer> addStringTransformer(List<Transformer> transformers) {
        if (transformers == null) {
            transformers = new ArrayList<>();
        }
        if (transformers.stream().noneMatch(t -> t instanceof StringToBooleanTransformer)) {
            transformers.listIterator().add(new StringToBooleanTransformer());
        }
        return transformers;
    }
}
