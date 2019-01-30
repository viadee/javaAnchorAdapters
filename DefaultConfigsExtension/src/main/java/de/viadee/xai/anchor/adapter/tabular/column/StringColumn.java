package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.MapBasedTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.ReplaceNullTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.io.Serializable;
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
    private static final long serialVersionUID = -7678371903173096032L;

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public StringColumn(String name) {
        this(name, null, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name             the column's name
     * @param replaceNullValue the value to replace null values with
     */
    public StringColumn(String name, String replaceNullValue) {
        this(name, Collections.singletonList(new ReplaceNullTransformer(replaceNullValue)), null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name              the column's name
     * @param mapTransformation a map used for transformations, see
     *                          {@link de.viadee.xai.anchor.adapter.tabular.transformations.MapBasedTransformer}
     */
    public StringColumn(String name, Map<Serializable, Serializable> mapTransformation) {
        this(name, Collections.singletonList(new MapBasedTransformer(mapTransformation)), null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name              the column's name
     * @param replaceNullValue  the value to replace null values with
     * @param mapTransformation a map used for transformations, see
     *                          {@link de.viadee.xai.anchor.adapter.tabular.transformations.MapBasedTransformer}
     */
    public StringColumn(String name, String replaceNullValue, Map<Serializable, Serializable> mapTransformation) {
        this(name, Arrays.asList(
                new ReplaceNullTransformer(replaceNullValue),
                new MapBasedTransformer(mapTransformation)),
                null,
                null);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param dataTransformer the transformation to apply
     * @param anchorTransformers the transformations to apply before discretization for anchor
     */
    public StringColumn(String name, Transformer dataTransformer, List<Transformer> anchorTransformers) {
        this(name, Collections.singletonList(dataTransformer), anchorTransformers, null);
    }

    /**
     * Instantiates the column
     *
     * @param name         the column's name
     * @param dataTransformers the transformations to apply
     * @param anchorTransformers the transformations to apply before discretization for anchor
     * @param discretizer  the discretizer to use
     */
    public StringColumn(String name, List<Transformer> dataTransformers, List<Transformer> anchorTransformers, Discretizer discretizer) {
        super(name, dataTransformers, anchorTransformers, (discretizer != null) ? discretizer : new UniqueValueDiscretizer());
    }
}
