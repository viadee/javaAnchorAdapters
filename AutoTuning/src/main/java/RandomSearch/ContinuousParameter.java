package RandomSearch;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Random;

public class ContinuousParameter extends Parameter {

    private double defaultValue;
    private double minValue;
    private double maxValue;
    private double currentValue;

    public ContinuousParameter(String name, double defaultValue, double minValue, double maxValue) {
        super(name);
        this.defaultValue = defaultValue;
        this.minValue = minValue;
        this.maxValue = maxValue;
    }

    public double getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(double defaultValue) {
        this.defaultValue = defaultValue;
    }

    public double getMinValue() {
        return minValue;
    }

    public void setMinValue(double minValue) {
        this.minValue = minValue;
    }

    public double getMaxValue() {
        return maxValue;
    }

    public void setMaxValue(double maxValue) {
        this.maxValue = maxValue;
    }

    public double getCurrentValue() {
        return currentValue;
    }

    public void setCurrentValue(double currentValue) {
        this.currentValue = currentValue;
    }

    public void searchRandom() {

        Random random = new Random();
        double randomDouble = this.minValue + (this.maxValue - this.minValue) * random.nextDouble();

        BigDecimal bd = new BigDecimal(randomDouble).setScale(2, RoundingMode.HALF_UP);
        double newParameter = bd.doubleValue();

        this.currentValue = newParameter;
    }
}
