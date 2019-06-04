package LossFunctions.Accuracy;

public class MetricFeature extends Feature {

    private double upperBound;
    private double lowerBound;

    public MetricFeature(String name, double upperBound, double lowerBound) {
        super(name);
        this.upperBound = upperBound;
        this.lowerBound = lowerBound;
    }

    public double getUpperBound() {
        return upperBound;
    }

    public void setUpperBound(double upperBound) {
        this.upperBound = upperBound;
    }

    public double getLowerBound() {
        return lowerBound;
    }

    public void setLowerBound(double lowerBound) {
        this.lowerBound = lowerBound;
    }
}
