package de.viadee.anchorj.tabular.column;

import java.util.Arrays;
import java.util.List;

import de.viadee.anchorj.tabular.discretizer.Discretizer;
import de.viadee.anchorj.tabular.discretizer.PercentileMedianDiscretizer;
import de.viadee.anchorj.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.anchorj.tabular.transformations.StringToDoubleTransformer;
import de.viadee.anchorj.tabular.transformations.Transformer;

/**
 * Column used for transforming String inputs into double values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by double values
 */
public class DoubleColumn extends NumberColumn {
    private static final long serialVersionUID = -5465215297043037654L;

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public DoubleColumn(String name) {
        this(name, null, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name               the column's name
     * @param dataTransformers   the object value to replace null values with. Must be convertible to double values
     * @param anchorTransformers the transformations to apply before discretization for anchor
     * @param discretizer        the discretizer to use
     */
    public DoubleColumn(String name, List<Transformer> dataTransformers, List<Transformer> anchorTransformers, Discretizer discretizer) {
        super(name, dataTransformers, anchorTransformers, discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name the column's name
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name) {
        return fromStringInput(name, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param discretizer the discretizer to use
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, Discretizer discretizer) {
        return fromStringInput(name, null, discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name       the column's name
     * @param classCount the count of classes to partition the data in
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, int classCount) {
        return fromStringInput(name, null, classCount);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param replaceNull the object value to replace null values with. Must be convertible to double values
     * @param discretizer the discretizer to use
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, Integer replaceNull, Discretizer discretizer) {
        return new DoubleColumn(
                name,
                Arrays.asList(createNullTransformer(replaceNull), new StringToDoubleTransformer()),
                null,
                (discretizer == null) ? new UniqueValueDiscretizer() : discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param replaceNull the object value to replace null values with. Must be convertible to double values
     * @param classCount  the count of classes to partition the data in
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, Integer replaceNull, int classCount) {
        return new DoubleColumn(name, Arrays.asList(createNullTransformer(replaceNull),
                new StringToDoubleTransformer()), null,
                (classCount < 1) ? new UniqueValueDiscretizer() : new PercentileMedianDiscretizer(classCount, replaceNull));
    }
}
