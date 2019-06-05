package LossFunctions.Accuracy;

public abstract class Feature {

    private String name;

    public Feature(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
