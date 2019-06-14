package RandomSearch;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import scala.reflect.internal.Trees;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class HyperparameterSpace {

    // performance indicators
    private double precision = 0;
    private double coverage = 0;
    private long runtime = 0;

    private List<Parameter> hyperParameters = new ArrayList<Parameter>();


    public HyperparameterSpace() {
        this.hyperParameters.add(new IntegerParameter("beamsize", 2, 1, 30));
        this.hyperParameters.add(new ContinuousParameter("tau", 1, 0.1, 1.0));
        this.hyperParameters.add(new ContinuousParameter("delta", 0.1, 0.1, 0.5));
        this.hyperParameters.add(new ContinuousParameter("epsilon", 0.1, 0.1, 0.5));
        this.hyperParameters.add(new ContinuousParameter("tauDiscrepancy", 0.05, 0.01, 0.1));
        this.hyperParameters.add(new IntegerParameter("initSampleCount", 1, 1, 10));
    }

    public void setRandomHyperparameterSpace() {

        for (Parameter p : hyperParameters) {
            p.searchRandom();
        }

    }

    /**
     * @param predictedValues
     * @param predictFunction
     * @param trainData
     * @return
     */
    public void calcPerformance(List<Integer> predictedValues, Function<TabularInstance, Integer> predictFunction, TabularInstance[] trainData) {
        int predInstances = 0;
        int matches = 0;
        for (int i = 0; i < trainData.length; i++) {
            if (!predictedValues.get(i).equals(-1)) {
                predInstances++;
                if (predictFunction.apply(trainData[i]).equals(predictedValues.get(i)))
                    matches++;
            }
        }

        double precision = (double) matches / (double) predInstances;
        double coverage = (double) predInstances / (double) predictedValues.size();

        this.precision = precision;
        this.coverage = coverage;

        System.out.println("==== Imitation accuracy of the global explanations is ====" + System.lineSeparator() +
                precision);
        System.out.println("==== Total Coverage of the global explanations is ====" + System.lineSeparator() +
                coverage);

    }

    public Parameter getParameterByName(String name){
        for (Parameter p : hyperParameters) {
            if (p.getName() == name) {
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

    public double getPrecision() {
        return precision;
    }

    public void setPrecision(double precision) {
        this.precision = precision;
    }

    public double getCoverage() {
        return coverage;
    }

    public void setCoverage(double coverage) {
        this.coverage = coverage;
    }
}
