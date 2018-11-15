package de.goerke.tobias.anchorj.tabular;

/**
 * @author ak902764
 */
public class FeatureValueMapping {

    private final TabularFeature feature;
    private final Object discretizedValue;

    public FeatureValueMapping(TabularFeature feature, Object discretizedValue) {
        this.feature = feature;
        this.discretizedValue = discretizedValue;
    }

    public TabularFeature getFeature() {
        return feature;
    }

    public Object getDiscretizedValue() {
        return discretizedValue;
    }
}
