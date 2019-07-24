package RandomSearch;

public interface ContinuousParameterInterface extends NumericalParameter {

    String type = "real";

    @Override
    Double getDefaultValue();

    @Override
    Double getCurrentValue();

    @Override
    Double getMinValue();

    @Override
    Double getMaxValue();

}
