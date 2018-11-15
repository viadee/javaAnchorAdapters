package de.goerke.tobias.anchorj.tabular;

/**
 * @author ak902764
 */
public class NativeValueMapping extends FeatureValueMapping {

    private final Object value;

    public NativeValueMapping(TabularFeature feature, Object value) {
        super(feature);
        this.value = value;
    }

    public Object getValue() {
        return value;
    }
}
