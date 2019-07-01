package RandomSearch;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class HyperparameterSpace {

    // performance indicators
    private double performance = 0;
    private double coverage = 0;
    private long runtime = 0;

    private List<Parameter> hyperParameters;


    public HyperparameterSpace() {
        this(null);
    }

    public HyperparameterSpace(List<Parameter> hyperParameters) {

        if (hyperParameters != null) {
            this.hyperParameters = hyperParameters;
        } else {
            this.hyperParameters = fillWithDefaults();
        }
    }

    private List<Parameter> fillWithDefaults() {

        List<Parameter> parameters = new ArrayList<Parameter>();

        parameters.add(new IntegerParameter("beamsize", 2, 1, 30));
        parameters.add(new ContinuousParameter("tau", 1, 0.1, 1.0));
        parameters.add(new ContinuousParameter("delta", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("epsilon", 0.1, 0.1, 0.5));
        parameters.add(new ContinuousParameter("tauDiscrepancy", 0.05, 0.01, 0.1));
        parameters.add(new IntegerParameter("initSampleCount", 1, 1, 10));

        return parameters;
    }

    public void setRandomHyperparameterSpace() {

        for (Parameter p : hyperParameters) {
            p.searchRandom();
        }

    }

    public Parameter getParameterByName(String name){
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
}
