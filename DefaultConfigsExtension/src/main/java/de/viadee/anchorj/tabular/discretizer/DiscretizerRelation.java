package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.Objects;

/**
 *
 */
public class DiscretizerRelation implements Serializable {
    private static final long serialVersionUID = -9212337107168284022L;

    public enum FeatureType {
        CATEGORICAL, METRIC, UNDEFINED
    }

    private final FeatureType featureType;

    private final int discretizedValue;

    private final Serializable categoricalValue;

    private final Double conditionMin;

    private final Double conditionMax;

    public DiscretizerRelation(int discretizedValue, Serializable categoricalValue) {
        this.featureType = FeatureType.CATEGORICAL;
        this.discretizedValue = discretizedValue;
        this.categoricalValue = categoricalValue;
        this.conditionMin = null;
        this.conditionMax = null;
    }

    public DiscretizerRelation(int discretizedValue, Double conditionMin, Double conditionMax) {
        this.featureType = FeatureType.METRIC;
        this.discretizedValue = discretizedValue;
        this.conditionMin = conditionMin;
        this.conditionMax = conditionMax;
        this.categoricalValue = null;
    }

    public FeatureType getFeatureType() {
        return featureType;
    }

    public int getDiscretizedValue() {
        return discretizedValue;
    }

    public Serializable getCategoricalValue() {
        return categoricalValue;
    }

    public Double getConditionMin() {
        return conditionMin;
    }

    public Double getConditionMax() {
        return conditionMax;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DiscretizerRelation that = (DiscretizerRelation) o;
        return discretizedValue == that.discretizedValue &&
                featureType == that.featureType &&
                Objects.equals(categoricalValue, that.categoricalValue) &&
                Objects.equals(conditionMin, that.conditionMin) &&
                Objects.equals(conditionMax, that.conditionMax);
    }

    @Override
    public int hashCode() {
        return Objects.hash(featureType, discretizedValue, categoricalValue, conditionMin, conditionMax);
    }

    @Override
    public String toString() {
        return "DiscretizerRelation{" +
                "featureType=" + featureType +
                ", discretizedValue=" + discretizedValue +
                ", categoricalValue=" + categoricalValue +
                ", conditionMin=" + conditionMin +
                ", conditionMax=" + conditionMax +
                '}';
    }
}
