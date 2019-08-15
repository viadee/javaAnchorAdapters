package Parameter;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Random;

public class ContinuousParameter implements ContinuousParameterInterface {

    private String name;
    private double defaultValue;
    private double minValue;
    private double maxValue;

    public ContinuousParameter(String name, double defaultValue, double minValue, double maxValue) {
        this.name = name;
        this.defaultValue = defaultValue;
        this.minValue = minValue;
        this.maxValue = maxValue;
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

    public Double getRandomValue() {
        Random random = new Random();
        double randomDouble = this.minValue + (this.maxValue - this.minValue) * random.nextDouble();

        BigDecimal bd = new BigDecimal(randomDouble).setScale(2, RoundingMode.HALF_UP);
        return bd.doubleValue();
    }

    public String getParameterString() {
        return name + " " + type + " [" + minValue + "," + maxValue + "][" + defaultValue + "]";
    }
}
