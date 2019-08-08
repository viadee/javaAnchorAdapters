package Parameter;

public interface Parameter {

    String getType();

    String getName();

    Object getDefaultValue();

    Object getCurrentValue();

    void searchRandom();

    Parameter copy();

    String getParameterString();

}
