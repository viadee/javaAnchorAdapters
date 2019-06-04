package LossFunctions.Accuracy;

import java.io.Serializable;

public class CategoricalFeature extends Feature {

    private Serializable value;

    public CategoricalFeature(String name, Serializable value) {
        super(name);
        this.value = value;
    }

    public Serializable getValue() {
        return value;
    }

    public void setValue(Serializable value) {
        this.value = value;
    }
}
