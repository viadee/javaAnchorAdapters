package de.viadee.anchorj.tabular;

import de.viadee.anchorj.DataInstance;
import de.viadee.anchorj.tabular.column.AbstractColumn;

import java.util.List;

/**
 * Represents an instance (i.e. row) of a data table
 */
public class TabularInstance implements DataInstance<Object[]> {
    private final Object[] instance;
    private AbstractColumn[] features;

    /**
     * Constructs the instance
     *
     * @param instance the instance array containing one element for each column
     */
    public TabularInstance(final AbstractColumn[] features, Object[] instance) {
        this.features = features;
        this.instance = instance;
    }

    /**
     * Constructs the instance
     *
     * @param instance the instance array containing one element for each column
     */
    public TabularInstance(final List<AbstractColumn> features, Object[] instance) {
        this.features = features.toArray(new AbstractColumn[0]);
        this.instance = instance;
    }

    /**
     * Copying constructor
     *
     * @param instance the instance to be copied
     */
    TabularInstance(TabularInstance instance) {
        Object[] copy = new Object[instance.getInstance().length];
        System.arraycopy(instance.getInstance(), 0, copy, 0, instance.getInstance().length);
        this.instance = copy;
        this.features = instance.features;
    }

    @Override
    public Object[] getInstance() {
        return instance;
    }

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
        if (featureIndex < 0) {
            throw new IllegalArgumentException("feature " + featureName + " not found");
        }

        return this.getValue(featureIndex);
    }

    /**
     * Returns the instance with discretized values
     *
     * @return the discretized instance
     */
    public Object[] asDiscretizedInstance() {
        Object[] result = new Object[instance.length];
        for (int i = 0; i < instance.length; i++) {
            final Object value = instance[i];
            if (features[i].getDiscretizer() == null) {
                result[i] = value;
            } else {
                result[i] = features[i].getDiscretizer().apply(value);
            }
        }
        return result;
    }

    public String getFeatureName(int index) {
        return features[index].getName();
//        return this.featureNames.entrySet().stream().filter(entry -> entry.getValue() == index).findFirst().orElseThrow(() ->
//                new ArrayIndexOutOfBoundsException("no entry found with index " + index)).getKey();
    }

    @Override
    public int getFeatureCount() {
        return instance.length;
    }
}
