package de.goerke.tobias.anchorj.tabular;

/**
 * @author ak902764
 */
public class CategoricalValueMapping extends FeatureValueMapping {
    private final Object categoricalValue;

    public CategoricalValueMapping(TabularFeature feature, Object discretizedValue, Object categoricalValue) {
        super(feature, discretizedValue);
        this.categoricalValue = categoricalValue;
    }

    public Object getCategoricalValue() {
        return categoricalValue;
    }

    @Override
    public String toString() {
        return this.getDiscretizedValue().toString();
    }
}
