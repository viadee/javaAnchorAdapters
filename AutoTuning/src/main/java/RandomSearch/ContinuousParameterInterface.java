package RandomSearch;

public interface ContinuousParameterInterface extends Parameter {

    @Override
    Double getDefaultValue();

    @Override
    Double getCurrentValue();

    @Override
    Double getMinValue();

    @Override
    Double getMaxValue();

}
