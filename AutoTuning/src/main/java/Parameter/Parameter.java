package Parameter;

import java.io.Serializable;

public interface Parameter {

    String getType();

    String getName();

    Object getDefaultValue();

    Object getCurrentValue();

    void searchRandom();

    Parameter copy();

}
