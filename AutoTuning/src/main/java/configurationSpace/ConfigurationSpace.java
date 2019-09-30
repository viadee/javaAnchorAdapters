package configurationSpace;

import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfigurationSpace;

import java.io.StringReader;

/**
 * The configuration space containing all parameters that need tuning and contains the performance for the selected
 * configuration
 */
public class ConfigurationSpace {

    private HyperparameterSpace hyperparameterSpace;
    private DiscretizationSpace discretizationSpace;

    // performance indicators
    private double performance = 0;
    private double coverage = 0;
    private long runtime = 0;

    public ConfigurationSpace(HyperparameterSpace hyperparameterSpace, DiscretizationSpace discretizationSpace) {
        this.hyperparameterSpace = hyperparameterSpace;
        this.discretizationSpace = discretizationSpace;
    }

    public ConfigurationSpace(HyperparameterSpace hyperparameterSpace) {
        this(hyperparameterSpace, null);
    }

    public ConfigurationSpace(DiscretizationSpace discretizationSpace) {
        this(null, discretizationSpace);
    }

    public ConfigurationSpace(ConfigurationSpace copyFrom) {
        this.runtime = copyFrom.getRuntime();
        this.coverage = copyFrom.getCoverage();
        this.performance = copyFrom.getPerformance();
        this.hyperparameterSpace = copyFrom.getHyperparameterSpace();
        this.discretizationSpace = copyFrom.getDiscretizationSpace();
    }

    public HyperparameterSpace getHyperparameterSpace() {
        return hyperparameterSpace;
    }

    public void setHyperparameterSpace(HyperparameterSpace hyperparameterSpace) {
        this.hyperparameterSpace = hyperparameterSpace;
    }

    public DiscretizationSpace getDiscretizationSpace() {
        return discretizationSpace;
    }

    public void setConfigurationSpace(DiscretizationSpace discretizationSpace) {
        this.discretizationSpace = discretizationSpace;
    }

    public double getPerformance() {
        return performance;
    }

    public void setPerformance(double performance) {
        this.performance = performance;
    }

    public double getCoverage() {
        return coverage;
    }

    public void setCoverage(double coverage) {
        this.coverage = coverage;
    }

    public long getRuntime() {
        return runtime;
    }

    public void setRuntime(long runtime) {
        this.runtime = runtime;
    }

    /**
     * Get the Create a ParameterConfigurationSpace for the SMAC algorithm
     *
     * @return ParameterConfigurationSpace
     */
    public ParameterConfigurationSpace toParameterConfigurationSpace() {
        StringBuffer stringBuffer = new StringBuffer(300);

        if (hyperparameterSpace != null) {
            stringBuffer
                    .append(hyperparameterSpace.getConfigurationString())
                    .append("\n");
        }

        if (discretizationSpace != null)
            stringBuffer.append(discretizationSpace.transferToConfigurationSpace());

        return new ParameterConfigurationSpace(new StringReader(stringBuffer.toString()));
    }
}
