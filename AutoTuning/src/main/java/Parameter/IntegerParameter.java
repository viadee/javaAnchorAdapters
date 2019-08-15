package Parameter;

import java.util.Random;

public class IntegerParameter implements IntegerParameterInterface {

    private String name;
    private int defaultValue;
    private int minValue;
    private int maxValue;

    public IntegerParameter(String name, int defaultValue, int minValue, int maxValue) {
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

    public Integer getDefaultValue() {
        return defaultValue;
    }

    public Integer getMinValue() {
        return minValue;
    }

    public Integer getMaxValue() {
        return maxValue;
    }

    public Integer getRandomValue() {
        Random random = new Random();
        return random.nextInt(this.maxValue - this.minValue) + 1 + this.minValue;
    }

    public String getParameterString() {
        return name + " " + type + " [" + minValue + "," + maxValue + "][" + defaultValue + "]";
    }
}
