package de.viadee.xai.anchor.adapter.tabular.column;

import java.util.Arrays;
import java.util.List;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.PercentileMedianDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.StringToIntTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

/**
 * Column used for transforming String inputs into integer values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by integer values
 */
public class IntegerColumn extends NumberColumn {
    private static final long serialVersionUID = 2779007741063221327L;

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public IntegerColumn(String name) {
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
    public IntegerColumn(String name, List<Transformer> dataTransformers, List<Transformer> anchorTransformers, Discretizer discretizer) {
        super(name, dataTransformers, anchorTransformers, discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name the column's name
     * @return the corresponding column object
     */
    public static IntegerColumn fromStringInput(String name) {
        return fromStringInput(name, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param discretizer the discretizer to use
     * @return the corresponding column object
     */
    public static IntegerColumn fromStringInput(String name, Discretizer discretizer) {
        return fromStringInput(name, null, discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name       the column's name
     * @param classCount the count of classes to partition the data in
     * @return the corresponding column object
     */
    public static IntegerColumn fromStringInput(String name, int classCount) {
        return fromStringInput(name, null, classCount);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param replaceNull the object value to replace null values with. Must be convertible to Integer values
     * @param discretizer the discretizer to use
     * @return the corresponding column object
     */
    public static IntegerColumn fromStringInput(String name, Integer replaceNull, Discretizer discretizer) {
        return new IntegerColumn(name, Arrays.asList(createNullTransformer(replaceNull), new StringToIntTransformer()),
                null, (discretizer == null) ? new UniqueValueDiscretizer() : discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param replaceNull the object value to replace null values with. Must be convertible to Integer values
     * @param classCount  the count of classes to partition the data in
     * @return the corresponding column object
     */
    public static IntegerColumn fromStringInput(String name, Integer replaceNull, int classCount) {
        return new IntegerColumn(name, Arrays.asList(createNullTransformer(replaceNull), new StringToIntTransformer()),
                null,
                (classCount == 0) ? new UniqueValueDiscretizer() : new PercentileMedianDiscretizer(classCount, replaceNull));
    }
}
