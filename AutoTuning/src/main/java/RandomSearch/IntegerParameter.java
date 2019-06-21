package RandomSearch;

import java.util.Random;

public class IntegerParameter implements IntegerParameterInterface {

    private String name;
    private int defaultValue;
    private int minValue;
    private int maxValue;
    private int currentValue;


    public IntegerParameter(String name, int defaultValue, int minValue, int maxValue) {
        this(name, defaultValue, minValue, maxValue, -1);
    }

    public IntegerParameter(String name, int currentValue) {
        this(name, -1, -1, -1, currentValue);
    }

    public IntegerParameter(String name, int defaultValue, int minValue, int maxValue, int currentValue) {
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

    public Integer getDefaultValue() {
        return defaultValue;
    }

    public Integer getMinValue() {
        return minValue;
    }

    public Integer getMaxValue() {
        return maxValue;
    }

    public Integer getCurrentValue() {
        return currentValue;
    }

    public void searchRandom() {

        Random random = new Random();
        int newParameter = random.nextInt(this.maxValue - this.minValue) + 1 + this.minValue;

        this.currentValue = newParameter;
    }
}
