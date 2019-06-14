package RandomSearch;

public interface Parameter {

    String getType();

    String getName();

    Number getMinValue();

    Number getMaxValue();

    Number getDefaultValue();

    Number getCurrentValue();

    void searchRandom();

}
