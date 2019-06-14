package RandomSearch;

public interface IntegerParameterInterface extends Parameter {

    String type = "integer";

    @Override
    Integer getDefaultValue();

    @Override
    Integer getCurrentValue();

    @Override
    Integer getMinValue();

    @Override
    Integer getMaxValue();
}
