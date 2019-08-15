package Parameter;

public interface NumericalParameter extends Parameter {

    @Override
    Number getDefaultValue();

    @Override
    Number getRandomValue();

    Number getMinValue();

    Number getMaxValue();

}
