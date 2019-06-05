package RandomSearch;

public class HyperparameterSpace {

    private double performance = 0;
    private long runtime = 0;

    private IntegerParameter beamSize;
    private ContinuousParameter tau;
    private ContinuousParameter delta;
    private ContinuousParameter epsilon;
    private ContinuousParameter tauDiscrepancy;
    private IntegerParameter initSampleCount;
    // more to come


    public HyperparameterSpace() {
        this.beamSize = new IntegerParameter("beamsize", 1, 30);
        this.tau = new ContinuousParameter("tau", 0.1, 1.0);
        this.delta = new ContinuousParameter("delta", 0.1, 0.5);
        this.epsilon = new ContinuousParameter("epsilon", 0.1, 0.5);
        this.tauDiscrepancy = new ContinuousParameter("tauDiscrepancy", 0.01, 0.1);
        this.initSampleCount = new IntegerParameter("initSampleCount", 1, 10);
    }

    public void setRandomHyperparameterSpace() {

        this.beamSize.searchRandom();
        this.tau.searchRandom();
        this.delta.searchRandom();
        this.epsilon.searchRandom();
        this.tauDiscrepancy.searchRandom();
        this.initSampleCount.searchRandom();

    }

    public IntegerParameter getBeamSize() {
        return beamSize;
    }

    public ContinuousParameter getTau() {
        return tau;
    }

    public ContinuousParameter getDelta() {
        return delta;
    }

    public ContinuousParameter getEpsilon() {
        return epsilon;
    }

    public ContinuousParameter getTauDiscrepancy() {
        return tauDiscrepancy;
    }

    public IntegerParameter getInitSampleCount() {
        return initSampleCount;
    }

    public long getRuntime() {
        return runtime;
    }

    public void setRuntime(long runtime) {
        this.runtime = runtime;
    }

    public double getPerformance() {
        return performance;
    }

    public void setPerformance(double performance) {
        this.performance = performance;
    }
}
