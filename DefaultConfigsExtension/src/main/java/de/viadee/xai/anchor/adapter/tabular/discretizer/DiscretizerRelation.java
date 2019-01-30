package de.viadee.xai.anchor.adapter.tabular.discretizer;

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

    private FeatureType featureType;

    private int discretizedValue;

    private Serializable categoricalValue;

    private Double conditionMin;

    private Double conditionMax;

    public DiscretizerRelation() {
    }

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

    public DiscretizerRelation(DiscretizerRelation copyRelation) {
        this.featureType = copyRelation.featureType;
        this.discretizedValue = copyRelation.discretizedValue;
        this.categoricalValue = copyRelation.categoricalValue;
        this.conditionMin = copyRelation.conditionMin;
        this.conditionMax = copyRelation.conditionMax;
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

    public void setFeatureType(FeatureType featureType) {
        this.featureType = featureType;
    }

    public void setDiscretizedValue(int discretizedValue) {
        this.discretizedValue = discretizedValue;
    }

    public void setCategoricalValue(Serializable categoricalValue) {
        this.categoricalValue = categoricalValue;
    }

    public void setConditionMin(Double conditionMin) {
        this.conditionMin = conditionMin;
    }

    public void setConditionMax(Double conditionMax) {
        this.conditionMax = conditionMax;
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
