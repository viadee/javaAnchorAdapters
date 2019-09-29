package configurationSpace;

public interface Parameter {

    String getType();

    String getName();

    Object getDefaultValue();

    Object getRandomValue();

    String getParameterString();

}
