package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.PercentileMedianDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.UniqueValueDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.StringToDoubleTransformer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.util.Arrays;
import java.util.List;

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
        this(name, null, null);
    }

    /**
     * Instantiates the column
     *
     * @param name               the column's name
     * @param transformers   the object value to replace null values with. Must be convertible to double values
     * @param discretizer        the discretizer to use
     */
    public DoubleColumn(String name, List<Transformer> transformers, Discretizer discretizer) {
        super(name, transformers, discretizer);
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
                Arrays.asList(createEmptyTransformator(replaceNull), new StringToDoubleTransformer()),
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
        return new DoubleColumn(name, Arrays.asList(createEmptyTransformator(replaceNull),
                new StringToDoubleTransformer()),
                (classCount < 1) ? new UniqueValueDiscretizer() : new PercentileMedianDiscretizer(classCount));
    }
}
