package configurationSpace;

public interface ContinuousParameterInterface extends NumericalParameter {

    String type = "real";

    @Override
    Double getDefaultValue();

    @Override
    Double getRandomValue();

    @Override
    Double getMinValue();

    @Override
    Double getMaxValue();

}
