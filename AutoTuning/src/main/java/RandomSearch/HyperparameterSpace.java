package RandomSearch;

import java.util.ArrayList;
import java.util.List;

public final class HyperparameterSpace {

    // performance indicators
    private double performance = 0;
    private double coverage = 0;
    private long runtime = 0;

    private final List<Parameter> hyperParameters;

    public HyperparameterSpace() {

        List<Parameter> parameters = new ArrayList<Parameter>();

        parameters.add(new IntegerParameter("beamsize", 2, 1, 30));
        parameters.add(new ContinuousParameter("tau", 1, 0.1, 1.0));
        parameters.add(new ContinuousParameter("delta", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("epsilon", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("tauDiscrepancy", 0.05, 0.01, 0.1));
        parameters.add(new IntegerParameter("initSampleCount", 1, 1, 10));

        this.hyperParameters = parameters;
    }

    public HyperparameterSpace(HyperparameterSpace copyFrom) {
        this.runtime = copyFrom.getRuntime();
        this.coverage = copyFrom.getCoverage();
        this.performance = copyFrom.getPerformance();
        this.hyperParameters = clone(copyFrom.getHyperParameters());
    }

    public HyperparameterSpace(List<Parameter> hyperParameters) {
        this.hyperParameters = hyperParameters;
    }

    public Parameter getParameterByName(String name) {
        for (Parameter p : hyperParameters) {
            if (p.getName().equals(name)) {
                return p;
            }
        }
        throw new RuntimeException("No parameter found by the name of " + name);
    }

    public List<Parameter> getHyperParameters() {
        return hyperParameters;
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

    public void setPerformance(double precision) {
        this.performance = precision;
    }

    public double getCoverage() {
        return coverage;
    }

    public void setCoverage(double coverage) {
        this.coverage = coverage;
    }

    public static List<Parameter> clone(List<Parameter> original) {
        List<Parameter> result = new ArrayList<>();
        for (Parameter p : original) {
            result.add(p.copy());
        }
        return result;
    }
}
