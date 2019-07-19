package de.viadee.xai.anchor.adapter.tabular;

import java.io.Serializable;
import java.util.Arrays;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.algorithm.DataInstance;

/**
 * Represents an instance (i.e. row) of a data table.
 * <p>
 * Holds both discretized and non-discretized values
 */
public class TabularInstance implements DataInstance<Serializable[]> {
    private static final long serialVersionUID = -2986158305227955506L;

    private final GenericColumn[] features;
    private final GenericColumn targetFeature;
    private final Serializable[] transformedInstance;
    private final Double[] discretizedInstance;
    private final Serializable transformedLabel;
    private final Double discretizedLabel;

    /**
     * Constructs the instance
     *
     * @param features            the features describing the instance / columns
     * @param targetFeature       the target feature describing the label
     * @param transformedInstance the instance array containing one element for each column.
     *                            Created before discretization and thus can be seen to represent original data
     * @param discretizedInstance the discretized values of the transformed instance
     * @param transformedLabel    the original label, if any
     * @param discretizedLabel    the discretized original label, if any
     */
    public TabularInstance(GenericColumn[] features, GenericColumn targetFeature, Serializable[] transformedInstance,
                           Double[] discretizedInstance, Serializable transformedLabel, Double discretizedLabel) {
        this.features = features;
        this.targetFeature = targetFeature;
        this.transformedInstance = transformedInstance;
        this.discretizedInstance = discretizedInstance;
        this.transformedLabel = transformedLabel;
        this.discretizedLabel = discretizedLabel;
    }

    /**
     * Copy constructor using {@link System#arraycopy}
     *
     * @param instanceToClone the instance to be cloned
     */
    public TabularInstance(TabularInstance instanceToClone) {
        this.features = instanceToClone.features;
        this.targetFeature = instanceToClone.targetFeature;
        this.discretizedLabel = null;
        this.transformedLabel = null;
        this.transformedInstance = new Serializable[instanceToClone.transformedInstance.length];
        this.discretizedInstance = new Double[instanceToClone.discretizedInstance.length];
        System.arraycopy(instanceToClone.transformedInstance, 0, this.transformedInstance, 0, transformedInstance.length);
        System.arraycopy(instanceToClone.discretizedInstance, 0, this.discretizedInstance, 0, discretizedInstance.length);
    }

    /**
     * @return the transformed instance created before discretization
     */
    public Serializable[] getTransformedInstance() {
        return transformedInstance;
    }

    @Override
    public Double[] getInstance() {
        return discretizedInstance;
    }

    public GenericColumn[] getFeatures() {
        return features;
    }

    /**
     * Gets the transformed value of a feature name
     *
     * @param featureName the feature's name
     * @return the corresponding value
     */
    public Serializable getTransformedValue(String featureName) {
        if (this.features == null) {
            throw new IllegalArgumentException("no feature names provided");
        }
        return getTransformedValue(this.getFeatureIndex(featureName));
    }

    private int getFeatureIndex(String featureName) {
        int featureIndex = -1;
        for (int i = 0; i < features.length; i++) {
            if (features[i].getName().equals(featureName)) {
                featureIndex = i;
                break;
            }
        }

        return featureIndex;
    }

    /**
     * @param feature the feature whose transformed value to obtain
     * @return the transformed value
     */
    public Serializable getTransformedValue(GenericColumn feature) {
        return getTransformedValue(Arrays.asList(features).indexOf(feature));
    }

    public Serializable getTransformedValue(int index) {
        return this.transformedInstance[index];
    }

    /**
     * @param feature the feature whose value to obtain
     * @return the discretized value
     */
    public Double getValue(GenericColumn feature) {
        return this.getValue(Arrays.asList(features).indexOf(feature));
    }

    /**
     * Gets the value of a feature name
     *
     * @param featureName the feature's name
     * @return the corresponding value
     */
    public Double getValue(String featureName) {
        if (this.features == null) {
            throw new IllegalArgumentException("no feature names provided");
        }
        return getValue(this.getFeatureIndex(featureName));
    }

    public Double getValue(int index) {
        return this.discretizedInstance[index];
    }

    @Override
    public int getFeatureCount() {
        return discretizedInstance.length;
    }

    /**
     * @return the target feature
     */
    public GenericColumn getTargetFeature() {
        return targetFeature;
    }

    /**
     * @return the transformed label
     */
    public Serializable getTransformedLabel() {
        return transformedLabel;
    }

    /**
     * @return the discretized label
     */
    public Double getDiscretizedLabel() {
        return discretizedLabel;
    }
}
