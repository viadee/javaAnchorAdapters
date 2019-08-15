package Parameter;

public interface IntegerParameterInterface extends NumericalParameter {

    String type = "integer";

    @Override
    Integer getDefaultValue();

    @Override
    Integer getRandomValue();

    @Override
    Integer getMinValue();

    @Override
    Integer getMaxValue();
}
