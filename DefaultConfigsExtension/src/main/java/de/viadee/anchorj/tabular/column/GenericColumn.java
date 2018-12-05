package de.viadee.anchorj.tabular.column;

import de.viadee.anchorj.tabular.discretizer.Discretizer;
import de.viadee.anchorj.tabular.transformations.Transformer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

/**
 * Represents the type of a column - whether the contained data is categorical or nominal
 */
public class GenericColumn implements Serializable {
    private final String name;
    private List<Transformer> transformations;
    private Discretizer discretizer;

    /**
     * @param name the column's name
     */
    public GenericColumn(String name) {
        this(name, new ArrayList<>(), null);
    }

    /**
     * @param name            the column's name
     * @param transformations the transformations to apply before discretization
     * @param discretizer     the discretization mapping the column to classes
     */
    public GenericColumn(String name, List<Transformer> transformations,
                         Discretizer discretizer) {
        this.name = name;
        this.transformations = (transformations == null) ? new ArrayList<>() : new ArrayList<>(transformations);
        this.discretizer = discretizer;
    }

    /**
     * Adds a transformation.
     * <p>
     * All transformations are executed by the list's order
     *
     * @param transformation the transformation to use
     * @return this object to use for further configuration
     */
    public GenericColumn addTransformation(Transformer transformation) {
        this.transformations.add(transformation);
        return this;
    }

    /**
     * Gets the name of the column
     *
     * @return the set name
     */
    public String getName() {
        return name;
    }

    /**
     * Uses the specified transformations to map the values to transformed results
     *
     * @param values the values to transform
     * @return the transformation's result
     */
    public Object[] transform(Object[] values) {
        if (transformations == null || transformations.isEmpty())
            return values;
        Object[] result = null;
        final ListIterator<Transformer> iter = transformations.listIterator();
        while (iter.hasNext()) {
            if (!iter.hasPrevious())
                result = iter.next().apply(values);
            else
                result = iter.next().apply(result);
        }
        return result;
    }

    /**
     * Gets the discretizer.
     *
     * @return the discretizer instance or null if not set
     */
    public Discretizer getDiscretizer() {
        return discretizer;
    }

    /**
     * Sets the discretizer
     *
     * @param discretizer the discretizer to use
     * @return this object to use for further configuration
     */
    public GenericColumn setDiscretizer(Discretizer discretizer) {
        this.discretizer = discretizer;
        return this;
    }

    /**
     * Specifies whether the column is to be contained in explanations
     *
     * @return true, if the column is used
     */
    public boolean isDoUse() {
        return true;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + " {" + name + "}";
    }
}
