package de.viadee.anchorj.tabular.column;

import de.viadee.anchorj.tabular.discretizer.Discretizer;
import de.viadee.anchorj.tabular.discretizer.PercentileMedianDiscretizer;
import de.viadee.anchorj.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.anchorj.tabular.transformations.StringToDoubleTransformer;
import de.viadee.anchorj.tabular.transformations.Transformer;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

/**
 * Column used for transforming String inputs into double values.
 * <p>
 * May e.g. be used when column is read from a CSV file and is represented by double values
 */
@SuppressWarnings({ "unused", "WeakerAccess" })
public class DoubleColumn extends NumberColumn {
    private static final long serialVersionUID = -5465215297043037654L;

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public DoubleColumn(String name, int originalColumnIndex) {
        this(name, originalColumnIndex, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name         the column's name
     * @param transformers the object value to replace null values with. Must be convertible to double values
     * @param discretizer  the discretizer to use
     */
    public DoubleColumn(String name, int originalColumnIndex, List<Transformer> transformers, Discretizer discretizer) {
        super(name, originalColumnIndex, transformers, discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name the column's name
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, int originalColumnIndex) {
        return fromStringInput(name, originalColumnIndex, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param discretizer the discretizer to use
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, int originalColumnIndex, Discretizer discretizer) {
        return fromStringInput(name, originalColumnIndex, null, discretizer);
    }

    /**
     * Instantiates the column
     *
     * @param name       the column's name
     * @param classCount the count of classes to partition the data in
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, int originalColumnIndex, int classCount) {
        return fromStringInput(name, originalColumnIndex, null, classCount);
    }

    /**
     * Instantiates the column
     *
     * @param name        the column's name
     * @param replaceNull the object value to replace null values with. Must be convertible to double values
     * @param discretizer the discretizer to use
     * @return the corresponding column object
     */
    public static DoubleColumn fromStringInput(String name, int originalColumnIndex, Serializable replaceNull, Discretizer discretizer) {
        return new DoubleColumn(name, originalColumnIndex, Arrays.asList(createNullTransformer(replaceNull), new StringToDoubleTransformer()),
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
    public static DoubleColumn fromStringInput(String name, int originalColumnIndex, Serializable replaceNull, int classCount) {
        return new DoubleColumn(name, originalColumnIndex, Arrays.asList(createNullTransformer(replaceNull),
                new StringToDoubleTransformer()),
                (classCount < 1) ? new UniqueValueDiscretizer() : new PercentileMedianDiscretizer(classCount));
    }
}
