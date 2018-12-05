package de.viadee.anchorj.tabular.column;

import de.viadee.anchorj.tabular.discretizer.Discretizer;
import de.viadee.anchorj.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.anchorj.tabular.transformations.MapBasedTransformer;
import de.viadee.anchorj.tabular.transformations.ReplaceNullTransformer;
import de.viadee.anchorj.tabular.transformations.Transformer;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * Column used for transforming String inputs into integer values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by integer values
 */
public class StringColumn extends NumberColumn {

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public StringColumn(String name) {
        this(name, null, (Discretizer) null);
    }

    /**
     * Instantiates the column
     *
     * @param name             the column's name
     * @param replaceNullValue the value to replace null values with
     */
    public StringColumn(String name, String replaceNullValue) {
        this(name, Collections.singletonList(new ReplaceNullTransformer(replaceNullValue)), null);
    }

    /**
     * Instantiates the column
     *
     * @param name              the column's name
     * @param mapTransformation a map used for transformations, see
     *                          {@link de.viadee.anchorj.tabular.transformations.MapBasedTransformer}
     */
    public StringColumn(String name, Map<String, String> mapTransformation) {
        this(name, Collections.singletonList(new MapBasedTransformer(mapTransformation)), null);
    }

    /**
     * Instantiates the column
     *
     * @param name              the column's name
     * @param replaceNullValue  the value to replace null values with
     * @param mapTransformation a map used for transformations, see
     *                          {@link de.viadee.anchorj.tabular.transformations.MapBasedTransformer}
     */
    public StringColumn(String name, String replaceNullValue, Map<String, String> mapTransformation) {
        this(name, Arrays.asList(
                new ReplaceNullTransformer(replaceNullValue),
                new MapBasedTransformer(mapTransformation)),
                null);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param transformer the transformation to apply
     */
    public StringColumn(String name, Transformer transformer) {
        this(name, Collections.singletonList(transformer), null);
    }

    /**
     * Instantiates the column
     *
     * @param name         the column's name
     * @param transformers the transformations to apply
     * @param discretizer  the discretizer to use
     */
    public StringColumn(String name, List<Transformer> transformers, Discretizer discretizer) {
        super(name, transformers, (discretizer != null) ? discretizer : new UniqueValueDiscretizer());
    }
}
