package de.goerke.tobias.anchorj.tabular;

import de.goerke.tobias.anchorj.DataInstance;

import java.util.Map;

/**
 * Represents an instance (i.e. row) of a data table
 */
public class TabularInstance implements DataInstance<Object[]> {
    private final Object[] instance;

    private final Map<String, Integer> featureNames;

    /**
     * Constructs the instance
     *
     * @param instance the instance array containing one element for each column
     */
    public TabularInstance(final Map<String, Integer> featureNames, Object[] instance) {
        this.featureNames = featureNames;
        this.instance = instance;
    }

    /**
     * Copying constructor
     *
     * @param instance the instance to be copied
     */
    TabularInstance(de.goerke.tobias.anchorj.tabular.TabularInstance instance) {
        Object[] copy = new Object[instance.getInstance().length];
        System.arraycopy(instance.getInstance(), 0, copy, 0, instance.getInstance().length);
        this.instance = copy;
        this.featureNames = instance.featureNames;
    }

    @Override
    public Object[] getInstance() {
        return instance;
    }

    public Object getFeature(String featureName) {
        if (this.featureNames == null) {
            throw new IllegalArgumentException("no feature names provided");
        }
        if (!this.featureNames.containsKey(featureName)) {
            throw new IllegalArgumentException("feature " + featureName + " not found");
        }

        return this.getFeature(this.featureNames.get(featureName));
    }

    @Override
    public int getFeatureCount() {
        return instance.length;
    }
}
