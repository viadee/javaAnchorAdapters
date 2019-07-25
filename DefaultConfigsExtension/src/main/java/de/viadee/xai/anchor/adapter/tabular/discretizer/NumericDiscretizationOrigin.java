package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;

/**
 * Represents a numeric discretization origin
 */
public class NumericDiscretizationOrigin extends DiscretizationOrigin {
    private Number minValue;
    private final Number maxValue;

    /**
     * Constructs the instance
     *
     * @param minValue range min value
     * @param maxValue range max value
     */
    public NumericDiscretizationOrigin(Number minValue, Number maxValue) {
        super(DiscretizationType.NUMERIC);
        if (minValue == null || maxValue == null) {
            throw new IllegalArgumentException("Both min and max value must not be null");
        }
        this.minValue = minValue;
        this.maxValue = maxValue;
    }

    @Override
    public boolean canDiscretize(Serializable originalValue) {
        if (!(originalValue instanceof Number)) {
            throw new IllegalArgumentException("Non-number type passed to numeric discretizer");
        }
        final Number value = (Number) originalValue;
        return (value.doubleValue() >= minValue.doubleValue() && value.doubleValue() < maxValue.doubleValue());
    }

    @Override
    public String outputFormat() {
        return "IN RANGE [" + getMinValue() + "," + getMaxValue() + ")";
    }

    /**
     * @return the min range value
     */
    public Number getMinValue() {
        return minValue;
    }

    /**
     * @return the max range value
     */
    public Number getMaxValue() {
        return maxValue;
    }

    /**
     *
     * @param minValue the new min value
     */
    public void setMinValue(Number minValue) {
        this.minValue = minValue;
    }

    @Override
    public String toString() {
        return "NumericDiscretizationOrigin [" + minValue + ", " + maxValue + ")";
    }
}
