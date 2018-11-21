package de.viadee.anchorj.tabular;

/**
 * @author ak902764
 */
public class CategoricalValueMapping extends DiscretizedValueMapping {

    private final Object categoricalValue;

    public CategoricalValueMapping(TabularFeature feature, Object discretizedValue, Object categoricalValue) {
        super(feature, discretizedValue);
        this.categoricalValue = categoricalValue;
    }

}
