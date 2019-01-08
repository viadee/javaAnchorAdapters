package de.viadee.anchorj.tabular.column;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import de.viadee.anchorj.tabular.discretizer.Discretizer;
import de.viadee.anchorj.tabular.transformations.Transformer;

/**
 * Represents the type of a column - whether the contained data is categorical or nominal
 */
public class GenericColumn implements Serializable {
    private static final long serialVersionUID = -7907742161569543398L;

    private final String name;
    private final List<Transformer> dataTransformations;
    private final List<Transformer> anchorTransformers;
    private Discretizer discretizer;

    /**
     * @param name the column's name
     */
    public GenericColumn(String name) {
        this(name, null, null, null);
    }

    /**
     * @param name            the column's name
     * @param dataTransformations the transformations to apply for the predict data
     * @param anchorTransformers the transformations to apply before discretization for anchor
     * @param discretizer     the discretization mapping the column to classes
     */
    public GenericColumn(String name, List<Transformer> dataTransformations, List<Transformer> anchorTransformers, Discretizer discretizer) {
        this.name = name;
        this.dataTransformations = (dataTransformations == null) ? new ArrayList<>() : new ArrayList<>(dataTransformations);
        this.anchorTransformers = (anchorTransformers == null) ? new ArrayList<>() : new ArrayList<>(anchorTransformers);
        this.discretizer = discretizer;
    }

    /**
     * Adds a transformation.
     * <p>
     * All dataTransformations are executed by the list's order
     *
     * @param transformation the transformation to use
     * @return this object to use for further configuration
     */
    public GenericColumn addDataTransformer(Transformer transformation) {
        this.dataTransformations.add(transformation);
        return this;
    }

    /**
     * Adds a predictTransformation.
     * <p>
     * All dataTransformations are executed by the list's order before calling predict. Useful when having NullTransformations to re transformData to a null value
     *
     * @param predictTransformation the predictTransformation to use
     * @return this object to use for further configuration
     */
    public GenericColumn addAnchorTransformer(Transformer predictTransformation) {
        this.dataTransformations.add(predictTransformation);
        return this;
    }

    public Serializable[] transformForAnchor(Serializable[] data) {
        return transform(this.anchorTransformers, data);
    }

    /**
     * Uses the specified dataTransformations to map the values to transformed results
     *
     * @param values the values to transformData
     * @return the transformation's result
     */
    public Serializable[] transformData(Serializable[] values) {
        return transform(this.dataTransformations, values);
    }

    public Serializable transformData(Serializable value) {
        return transform(this.dataTransformations, new Serializable[]{value})[0];
    }

    private static Serializable[] transform(List<Transformer> transformations, Serializable[] data) {
        Serializable[] result = data;
        if (transformations != null && !transformations.isEmpty()) {
            for (Transformer transformation : transformations) {
                result = transformation.apply(result);
            }
        }

        return result;
    }

    /**
     * Gets the name of the column
     *
     * @return the set name
     */
    public String getName() {
        return name;
    }

    public List<Transformer> getDataTransformations() {
        return dataTransformations;
    }

    public List<Transformer> getAnchorTransformations() {
        return dataTransformations;
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
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        GenericColumn that = (GenericColumn) o;
        return Objects.equals(name, that.name) &&
                Objects.equals(dataTransformations, that.dataTransformations) &&
                Objects.equals(anchorTransformers, that.anchorTransformers) &&
                Objects.equals(discretizer, that.discretizer);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, dataTransformations, anchorTransformers, discretizer);
    }

    @Override
    public String toString() {
        return "GenericColumn{" +
                "name='" + name + '\'' +
                ", dataTransformations=" + dataTransformations +
                ", anchorTransformers=" + anchorTransformers +
                ", discretizer=" + discretizer +
                '}';
    }
}
