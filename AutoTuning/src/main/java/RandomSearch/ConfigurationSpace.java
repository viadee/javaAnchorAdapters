package RandomSearch;

public class ConfigurationSpace {

    private HyperparameterSpace hyperparameterSpace;
    private ConfigurationSpace configurationSpace;

    // performance indicators
    private double performance = 0;
    private double coverage = 0;
    private long runtime = 0;

    public ConfigurationSpace() {
    }

    public ConfigurationSpace(HyperparameterSpace hyperparameterSpace, ConfigurationSpace configurationSpace) {
        this.hyperparameterSpace = hyperparameterSpace;
        this.configurationSpace = configurationSpace;
    }

    public HyperparameterSpace getHyperparameterSpace() {
        return hyperparameterSpace;
    }

    public void setHyperparameterSpace(HyperparameterSpace hyperparameterSpace) {
        this.hyperparameterSpace = hyperparameterSpace;
    }

    public ConfigurationSpace getConfigurationSpace() {
        return configurationSpace;
    }

    public void setConfigurationSpace(ConfigurationSpace configurationSpace) {
        this.configurationSpace = configurationSpace;
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
}
