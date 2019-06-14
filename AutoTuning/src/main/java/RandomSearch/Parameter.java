package RandomSearch;

public interface Parameter {

    String getName();

    Number getMinValue();

    Number getMaxValue();

    Number getDefaultValue();

    Number getCurrentValue();

    void searchRandom();

}
