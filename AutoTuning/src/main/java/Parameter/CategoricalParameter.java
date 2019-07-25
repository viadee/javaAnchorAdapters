package Parameter;

import java.util.Random;

public class CategoricalParameter implements Parameter {

    String type = "categorical";

    private String name;
    private String[] allValues;
    private String defaultValue;
    private String currentValue;


    public CategoricalParameter(String name, String currentValue) {
        this(name, currentValue, currentValue, new String[]{currentValue});
    }

    public CategoricalParameter(String name, String[] allValues) {
        this(name, allValues.length != 0 ? allValues[0] : null, allValues.length != 0 ? allValues[0] : null, allValues);
    }

    public CategoricalParameter(String name, String defaultValue, String currentValue, String[] allValues) {
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

    public String getDefaultValue() {
        return defaultValue;
    }

    public String getCurrentValue() {
        return currentValue;
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
