package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;

/**
 * Represents the type of a column - whether the contained data is categorical or nominal
 */
public class GenericColumn implements Serializable {
    private static final long serialVersionUID = -7907742161569543398L;

    private final String name;
    private final List<Transformer> transformations;
    private Discretizer discretizer;

    private Consumer<String[]> postBuildListener;

    /**
     * @param name the column's name
     */
    public GenericColumn(String name) {
        this(name, null, null);
    }

    /**
     * @param name            the column's name
     * @param transformations the transformations to apply for the predict data
     * @param discretizer     the discretization mapping the column to classes
     */
    public GenericColumn(String name, List<Transformer> transformations, Discretizer discretizer) {
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
    public GenericColumn addTransformer(Transformer transformation) {
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

    public List<Transformer> getTransformers() {
        return transformations;
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

    /**
     * The subclass may register a post build listener which gets fed with the data when build is called
     * <p>
     * This allows the column to configure itself based on the data existing in the dataset
     *
     * @return the post build listener or null
     */
    public Consumer<String[]> getPostBuildListener() {
        return postBuildListener;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GenericColumn that = (GenericColumn) o;
        return Objects.equals(name, that.name) &&
                Objects.equals(transformations, that.transformations) &&
                Objects.equals(discretizer, that.discretizer);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, transformations, discretizer);
    }

    @Override
    public String toString() {
        return "GenericColumn{" +
                "name='" + name + '\'' +
                ", transformations=" + transformations +
                ", discretizer=" + discretizer +
                '}';
    }
}
