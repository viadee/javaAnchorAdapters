package RandomSearch;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Random;

public class ContinuousParameter implements ContinuousParameterInterface {

    private String name;
    private double defaultValue;
    private double minValue;
    private double maxValue;
    private double currentValue;

    public ContinuousParameter(String name, double defaultValue, double minValue, double maxValue) {
        this(name, defaultValue, minValue, maxValue, defaultValue);
    }

    public ContinuousParameter(String name, double currentValue) {
        this(name, -1, -1, -1 , currentValue);
    }

    public ContinuousParameter(String name, double defaultValue, double minValue, double maxValue, double currentValue) {
        this.name = name;
        this.defaultValue = defaultValue;
        this.minValue = minValue;
        this.maxValue = maxValue;
        this.currentValue = currentValue;
    }


    public String getType() {
        return type;
    }

    public String getName() {
        return name;
    }

    public Double getDefaultValue() {
        return defaultValue;
    }

    public Double getMinValue() {
        return minValue;
    }

    public Double getMaxValue() {
        return maxValue;
    }

    public Double getCurrentValue() {
        return currentValue;
    }

    public void searchRandom() {

        Random random = new Random();
        double randomDouble = this.minValue + (this.maxValue - this.minValue) * random.nextDouble();

        BigDecimal bd = new BigDecimal(randomDouble).setScale(2, RoundingMode.HALF_UP);
        double newParameter = bd.doubleValue();

        this.currentValue = newParameter;
    }

    public ContinuousParameter copy() {
        return new ContinuousParameter(name, defaultValue, minValue, maxValue, currentValue);
    }
}
