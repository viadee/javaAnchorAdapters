package de.viadee.anchorj.tabular;

/**
 * TODO why subclasses? Held objects are all of type Object
 *
 * @author ak902764
 */
public abstract class FeatureValueMapping {
    private final TabularFeature feature;
    private final Object value;

    public FeatureValueMapping(TabularFeature feature, Object value) {
        this.feature = feature;
        this.value = value;
    }

    public TabularFeature getFeature() {
        return feature;
    }

    public Object getValue() {
        return value;
    }

}
