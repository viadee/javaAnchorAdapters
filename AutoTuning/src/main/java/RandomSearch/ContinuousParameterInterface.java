package RandomSearch;

public interface ContinuousParameterInterface extends Parameter {

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
