package Parameter;

import java.util.Arrays;
import java.util.Random;

public class CategoricalParameter implements Parameter {

    String type = "categorical";

    private String name;
    private String[] allValues;
    private String defaultValue;


    public CategoricalParameter(String name, String[] allValues) {
        this(name, allValues.length != 0 ? allValues[0] : null, allValues);
    }

    public CategoricalParameter(String name, String defaultValue, String[] allValues) {
        if (defaultValue == null){
            throw new IllegalArgumentException("Categorical parameters need at least one category");
        }
        this.allValues = allValues;
        this.name = name;
        this.defaultValue = defaultValue;
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

    public String[] getAllValues() {
        return allValues;
    }

    public String getRandomValue() {
        Random random = new Random();
        int newParameter = random.nextInt(allValues.length);

        return allValues[newParameter];
    }
    
    public String getParameterString() {
        String[] allValues = Arrays.stream(this.allValues).map(str -> str.toString()).toArray(String[]::new);
        return name + " " + type + " {" + String.join(",", allValues) + "}[" + defaultValue + "]";
    }
}
