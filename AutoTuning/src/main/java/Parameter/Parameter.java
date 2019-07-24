package Parameter;

import java.io.Serializable;

public interface Parameter {

    String getType();

    String getName();

    Serializable getDefaultValue();

    Serializable getCurrentValue();

    void searchRandom();

    Parameter copy();

}
