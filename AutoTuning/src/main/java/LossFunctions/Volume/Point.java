package LossFunctions.Volume;

public class Point {

    private double coverage;
    private double precision;

    public Point(double coverage, double precision){
        this.precision = precision;
        this.coverage = coverage;
    }

    public void setPrecision(double precision) {
        this.precision = precision;
    }

    public void setCoverage(double coverage) {
        this.coverage = coverage;
    }

    public double getPrecision() {
        return precision;
    }

    public double getCoverage() {
        return coverage;
    }

}
