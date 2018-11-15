package de.goerke.tobias.anchorj.tabular;

/**
 * @author ak902764
 */
public class DiscretizedValueMapping extends FeatureValueMapping {
    private final Object discretizedValue;

    public DiscretizedValueMapping(TabularFeature feature, Object discretizedValue) {
        super(feature);
        this.discretizedValue = discretizedValue;
    }

    public Object getDiscretizedValue() {
        return discretizedValue;
    }
}
