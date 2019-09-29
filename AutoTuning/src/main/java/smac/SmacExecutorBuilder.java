package smac;

import configurationSpace.ConfigurationSpace;
import ca.ubc.cs.beta.aeatk.objectives.RunObjective;
import ca.ubc.cs.beta.aeatk.smac.SMACOptions;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluator;

import java.io.File;

/**
 * The builder class for the smac optimization method.
 * <p></p>
 * Set various needed values to build an instance of the SmacExecutor class.
 */
public class SmacExecutorBuilder {

    private String trainInstancesFile;
    private String testInstancesFile;
    private ConfigurationSpace configurationSpace;
    private SMACOptions smacOptions;
    private TargetAlgorithmEvaluator tae;

    /*
     * default values
     */
    private String outputDir = System.getProperty("user.dir") + File.separator + "smac-output" + File.separator;
    private String scenario = String.valueOf(System.currentTimeMillis());
    private int totalNumRunsLimit = 20;
    private RunObjective runObjective = RunObjective.QUALITY;
    private boolean logModel = false;
    private double cutOffTime = Double.MAX_VALUE;

    public SmacExecutorBuilder() {
    }

    /**
     * Sets the configuration space with the parameters to optimize
     *
     * @param configurationSpace configuration space with hyperparameters and discretization space
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setConfigurationSpace(ConfigurationSpace configurationSpace) {
        this.configurationSpace = configurationSpace;
        return this;
    }

    /**
     * Set the scenario name for logging
     *
     * @param scenario the name of the current scenario
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setScenario(String scenario) {
        this.scenario = scenario;
        return this;
    }

    /**
     * Set the target algorithm for optimization
     *
     * @param tae the algorithm to optimize
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setTargetAlgorithmEvaluator(TargetAlgorithmEvaluator tae) {
        this.tae = tae;
        return this;
    }

    /**
     * Set the directory where the output is logged to
     *
     * @param outputDir the directory name for output
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setOutputDir(String outputDir) {
        this.outputDir = outputDir;
        return this;
    }

    /**
     * Set the file path and name of the train instances
     *
     * @param trainInstancesFile the full file path of the instances file
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setTrainInstancesFile(String trainInstancesFile) {
        this.trainInstancesFile = trainInstancesFile;
        return this;
    }

    /**
     * Set the file path and name of the test instances
     *
     * @param testInstancesFile the full file path of the test instances file
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setTestInstancesFile(String testInstancesFile) {
        this.testInstancesFile = testInstancesFile;
        return this;
    }

    /**
     * Set the total number of executions that should be executed
     *
     * @param totalNumRunsLimit the total number of executions
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setTotalNumRunsLimit(int totalNumRunsLimit) {
        this.totalNumRunsLimit = totalNumRunsLimit;
        return this;
    }

    /**
     * Set the objective the run is optimized on
     *
     * @param runObjective the objective of optimization
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setRunObjective(RunObjective runObjective) {
        this.runObjective = runObjective;
        return this;
    }

    /**
     * Set whether the created model should be logged
     *
     * @param logModel should the model be logged
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setLogModel(boolean logModel) {
        this.logModel = logModel;
        return this;
    }

    /**
     * Set the cut-off time for the algorithm runs
     *
     * @param cutOffTime the time after which the algorithm is terminated
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setCutOffTime(double cutOffTime) {
        this.cutOffTime = cutOffTime;
        return this;
    }

    /**
     * Set a full set of options for smac
     *
     * @param smacOptions the options for the smac algorithm
     * @return the current {@link SmacExecutorBuilder} for chaining
     */
    public SmacExecutorBuilder setSmacOptions(SMACOptions smacOptions) {
        this.smacOptions = smacOptions;
        return this;
    }

    private void prepareForBuild() {
        if (smacOptions == null)
            this.smacOptions = new SMACOptions();

        smacOptions.scenarioConfig._runObj = this.runObjective;
        smacOptions.scenarioConfig.instanceOptions.useInstances = false;
        smacOptions.scenarioConfig.limitOptions.totalNumRunsLimit = this.totalNumRunsLimit;
        smacOptions.stateOpts.saveContextWithState = false;
        smacOptions.randomForestOptions.logModel = this.logModel;

        // instance options
        smacOptions.scenarioConfig.instanceOptions.instanceFile = this.trainInstancesFile;
        smacOptions.scenarioConfig.instanceOptions.testInstanceFile = this.testInstancesFile;
    }

    /**
     * Build the smac Helper instance setting all values or their pre-configures default values.
     *
     * @return the smac Helper instance
     */
    public SmacExecutor build() {
        prepareForBuild();
        return new SmacExecutor(scenario, configurationSpace, tae, outputDir, smacOptions, cutOffTime);
    }
}
