package RandomSearch;

import java.util.Random;

public class IntegerParameter extends Parameter {

    private int minValue;
    private int maxValue;
    private int currentValue;

    public IntegerParameter(String name, int minValue, int maxValue) {
        super(name);
        this.minValue = minValue;
        this.maxValue = maxValue;
    }

    public int getMinValue() {
        return minValue;
    }

    public void setMinValue(int minValue) {
        this.minValue = minValue;
    }

    public int getMaxValue() {
        return maxValue;
    }

    public void setMaxValue(int maxValue) {
        this.maxValue = maxValue;
    }

    public int getCurrentValue() {
        return currentValue;
    }

    public void setCurrentValue(int currentValue) {
        this.currentValue = currentValue;
    }

    public void searchRandom() {

        Random random = new Random();
        int newParameter = random.nextInt(this.maxValue - this.minValue) + 1 + this.minValue;

        this.currentValue = newParameter;
    }
}
