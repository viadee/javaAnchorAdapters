package Parameter;

import java.util.Random;

public class CategoricalParameter implements Parameter {

    String type = "categorical";

    private String name;
    private Object[] allValues;
    private Object defaultValue;
    private Object currentValue;


    public CategoricalParameter(String name, Object currentValue) {
        this(name, currentValue, currentValue, new Object[]{currentValue});
    }

    public CategoricalParameter(String name, Object[] allValues) {
        this(name, allValues.length != 0 ? allValues[0] : null, allValues.length != 0 ? allValues[0] : null, allValues);
    }

    public CategoricalParameter(String name, Object defaultValue, Object currentValue, Object[] allValues) {
        if (defaultValue == null){
            throw new IllegalArgumentException("Categorical parameters need at least one category");
        }
        this.allValues = allValues;
        this.name = name;
        this.defaultValue = defaultValue;
        this.currentValue = currentValue;
    }

    public String getType() {
        return type;
    }

    public String getName() {
        return name;
    }

    public Object getDefaultValue() {
        return defaultValue;
    }

    public Object getCurrentValue() {
        return currentValue;
    }

    public Object[] getAllValues() {
        return allValues;
    }

    public void searchRandom() {
        Random random = new Random();
        int newParameter = random.nextInt(allValues.length);

        currentValue = allValues[newParameter];
    }

    public CategoricalParameter copy() {
        return new CategoricalParameter(name, defaultValue, currentValue, allValues);
    }
}
