package RandomSearch;

public interface IntegerParameterInterface extends Parameter {

    @Override
    Integer getDefaultValue();

    @Override
    Integer getCurrentValue();

    @Override
    Integer getMinValue();

    @Override
    Integer getMaxValue();
}
