package RandomSearch;

public interface NumericalParameter extends Parameter{

    @Override
    Number getDefaultValue();

    @Override
    Number getCurrentValue();

    Number getMinValue();

    Number getMaxValue();

}
