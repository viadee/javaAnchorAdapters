package de.goerke.tobias.anchorj.tabular;

/**
 * @author ak902764
 */
public class MetricValueMapping extends FeatureValueMapping {
    private final Double minValue;
    private final Double maxValue;

    public MetricValueMapping(TabularFeature feature, Object discretizedValue, Double minValue, Double maxValue) {
        super(feature, discretizedValue);

        this.minValue = minValue;
        this.maxValue = maxValue;
    }

    public Double getMinValue() {
        return minValue;
    }

    public Double getMaxValue() {
        return maxValue;
    }

    @Override
    public String toString() {
        return "Range(" + this.minValue + ", " + this.maxValue + ")";
    }

}
