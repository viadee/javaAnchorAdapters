package de.viadee.anchorj.tabular.column;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Objects;

import de.viadee.anchorj.tabular.discretizer.Discretizer;
import de.viadee.anchorj.tabular.transformations.Transformer;

/**
 * Represents the type of a column - whether the contained data is categorical or nominal
 */
public class GenericColumn implements Serializable {
    private static final long serialVersionUID = -7907742161569543398L;

    private final String name;
    private List<Transformer> transformations;
    private Discretizer discretizer;

    private final int originalColumnIndex;

    /**
     * @param name the column's name
     */
    @SuppressWarnings("unused")
    public GenericColumn(String name, int originalColumnIndex) {
        this(name, originalColumnIndex, new ArrayList<>(), null);
    }

    /**
     * @param name            the column's name
     * @param transformations the transformations to apply before discretization
     * @param discretizer     the discretization mapping the column to classes
     */
    @SuppressWarnings("WeakerAccess")
    public GenericColumn(String name, int originalColumnIndex, List<Transformer> transformations, Discretizer discretizer) {
        this.name = name;
        this.transformations = (transformations == null) ? new ArrayList<>() : new ArrayList<>(transformations);
        this.discretizer = discretizer;
        this.originalColumnIndex = originalColumnIndex;
    }

    /**
     * Adds a transformation.
     * <p>
     * All transformations are executed by the list's order
     *
     * @param transformation the transformation to use
     * @return this object to use for further configuration
     */
    @SuppressWarnings("unused")
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
    public Serializable[] transform(Serializable[] values) {
        if (transformations == null || transformations.isEmpty()) {
            return values;
        }
        Serializable[] result = null;
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

    public int getOriginalColumnIndex() {
        return originalColumnIndex;
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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GenericColumn that = (GenericColumn) o;
        return originalColumnIndex == that.originalColumnIndex &&
                Objects.equals(name, that.name) &&
                Objects.equals(transformations, that.transformations) &&
                Objects.equals(discretizer, that.discretizer);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, transformations, discretizer, originalColumnIndex);
    }

    @Override
    public String toString() {
        return "GenericColumn{" +
                "name='" + name + '\'' +
                ", transformations=" + transformations +
                ", discretizer=" + discretizer +
                ", originalColumnIndex=" + originalColumnIndex +
                '}';
    }
}
