package RandomSearch;

public class HyperparameterSpace {

    private double performance = 0;

    private IntegerParameter beamSize;
    private ContinuousParameter tau;
    // more to come


    public HyperparameterSpace() {
        this.beamSize  = new IntegerParameter("beamsize", 1, 30);
        this.tau = new ContinuousParameter("tau", 0.1, 1.0);
        this.performance = 0;
    }

    public void setRandomHyperparameterSpace() {

        this.beamSize.searchRandom();
        this.tau.searchRandom();

    }

    public IntegerParameter getBeamSize() {
        return beamSize;
    }

    public ContinuousParameter getTau() {
        return tau;
    }

    public double getPerformance() {
        return performance;
    }

    public void setPerformance(double performance) {
        this.performance = performance;
    }
}
