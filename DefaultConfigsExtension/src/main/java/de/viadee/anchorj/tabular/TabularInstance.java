package de.viadee.anchorj.tabular;

import de.viadee.anchorj.DataInstance;
import de.viadee.anchorj.tabular.column.GenericColumn;

import java.util.Arrays;

/**
 * Represents an instance (i.e. row) of a data table.
 * <p>
 * Holds both discretized and non-discretized values
 */
public class TabularInstance implements DataInstance<Object[]> {
    private final GenericColumn[] features;
    private final GenericColumn targetFeature;
    private final Object[] transformedInstance;
    private final Integer[] discretizedInstance;
    private final Object transformedLabel;
    private final Integer discretizedLabel;

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
    public TabularInstance(GenericColumn[] features, GenericColumn targetFeature, Object[] transformedInstance,
                           Integer[] discretizedInstance, Object transformedLabel, Integer discretizedLabel) {
        this.features = features;
        this.targetFeature = targetFeature;
        this.transformedInstance = transformedInstance;
        this.discretizedInstance = discretizedInstance;
        this.transformedLabel = transformedLabel;
        this.discretizedLabel = discretizedLabel;
    }

    /**
     * Copy constructor using System.arraycopy
     *
     * @param instanceToClone the instance to be cloned
     */
    public TabularInstance(TabularInstance instanceToClone) {
        this.features = instanceToClone.features;
        this.targetFeature = instanceToClone.targetFeature;
        this.discretizedLabel = null;
        this.transformedLabel = null;
        this.transformedInstance = new Object[instanceToClone.transformedInstance.length];
        this.discretizedInstance = new Integer[instanceToClone.discretizedInstance.length];
        System.arraycopy(instanceToClone.transformedInstance, 0, this.transformedInstance, 0, transformedInstance.length);
        System.arraycopy(instanceToClone.discretizedInstance, 0, this.discretizedInstance, 0, discretizedInstance.length);
    }

    /**
     * @return the transformed instance created before discretization
     */
    public Object[] getTransformedInstance() {
        return transformedInstance;
    }

    @Override
    public Integer[] getInstance() {
        return discretizedInstance;
    }

    public GenericColumn[] getFeatures() {
        return features;
    }

    /**
     * @param feature the feature whose transformed value to obtain
     * @return the transformed value
     */
    public Object getTransformedValue(GenericColumn feature) {
        final int index = Arrays.asList(features).indexOf(feature);
        if (index < 0)
            throw new IllegalArgumentException("Feature not existant");
        return transformedInstance[index];
    }

    /**
     * Gets the transformed value of a feature name
     *
     * @param featureName the feature's name
     * @return the corresponding value
     */
    public Object getTransformedValue(String featureName) {
        if (this.features == null) {
            throw new IllegalArgumentException("no feature names provided");
        }
        int featureIndex = -1;
        for (int i = 0; i < features.length; i++) {
            if (features[i].getName().equals(featureName)) {
                featureIndex = i;
                break;
            }
        }
        return getTransformedValue(features[featureIndex]);
    }

    /**
     * @param feature the feature whose value to obtain
     * @return the discretized value
     */
    public Integer getValue(GenericColumn feature) {
        final int index = Arrays.asList(features).indexOf(feature);
        if (index < 0)
            throw new IllegalArgumentException("Feature not existant");
        return discretizedInstance[index];
    }

    /**
     * Gets the value of a feature name
     *
     * @param featureName the feature's name
     * @return the corresponding value
     */
    public Integer getValue(String featureName) {
        if (this.features == null) {
            throw new IllegalArgumentException("no feature names provided");
        }
        int featureIndex = -1;
        for (int i = 0; i < features.length; i++) {
            if (features[i].getName().equals(featureName)) {
                featureIndex = i;
                break;
            }
        }
        return getValue(features[featureIndex]);
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
    public Object getTransformedLabel() {
        return transformedLabel;
    }

    /**
     * @return the discretized label
     */
    public Integer getDiscretizedLabel() {
        return discretizedLabel;
    }
}
