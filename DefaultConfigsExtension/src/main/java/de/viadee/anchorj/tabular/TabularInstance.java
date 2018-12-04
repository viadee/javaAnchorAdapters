package de.viadee.anchorj.tabular;

import de.viadee.anchorj.DataInstance;
import de.viadee.anchorj.tabular.column.AbstractColumn;

import java.util.Arrays;

/**
 * Represents an instance (i.e. row) of a data table.
 * <p>
 * Holds both discretized and non-discretized values
 */
public class TabularInstance implements DataInstance<Object[]> {
    private final AbstractColumn[] features;
    private final Object[] transformedInstance;
    private final Object[] discretizedInstance;
    private final Integer originalLabel;

    /**
     * Constructs the instance
     *
     * @param features
     * @param transformedInstance the instance array containing one element for each column.
     *                            Created before discretization and thus can be seen to represent original data
     * @param discretizedInstance the discretized values of the transformed instance
     * @param originalLabel       the original label, if any
     */
    public TabularInstance(AbstractColumn[] features, Object[] transformedInstance, Object[] discretizedInstance, Integer originalLabel) {
        this.features = features;
        this.transformedInstance = transformedInstance;
        this.discretizedInstance = discretizedInstance;
        this.originalLabel = originalLabel;
    }

    /**
     * Copy constructor using System.arraycopy
     *
     * @param instanceToClone the instance to be cloned
     */
    public TabularInstance(TabularInstance instanceToClone) {
        this.features = instanceToClone.getFeatures();
        this.originalLabel = null;
        this.transformedInstance = new Object[instanceToClone.transformedInstance.length];
        this.discretizedInstance = new Object[instanceToClone.discretizedInstance.length];
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
    public Object[] getInstance() {
        return discretizedInstance;
    }

    public AbstractColumn[] getFeatures() {
        return features;
    }

    private Object getValue(AbstractColumn feature) {
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
    public Object getValue(String featureName) {
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
     * @return the original label, if set. null otherwise
     */
    public Integer getOriginalLabel() {
        return originalLabel;
    }
}
