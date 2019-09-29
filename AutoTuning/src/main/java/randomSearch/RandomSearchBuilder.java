package randomSearch;

import dataInitialization.DataInitializer;
import evaluationMetrics.PerformanceMeasures;
import configurationSpace.ConfigurationSpace;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;

import java.util.function.Function;

/**
 * The builder class for the random search optimization method for the Anchors explanations.
 * <p></p>
 * Set various needed values to build an instance of the randomSearch class.
 */
public class RandomSearchBuilder {

    private int[] explainedInstanceIndex;
    private AnchorTabular anchorTabular;
    private DataInitializer data;
    private Function<TabularInstance, Integer> classificationFunction;
    private ConfigurationSpace configurationSpace;
    private long timeTermination;
    private int executionTermination;

    /*
     * default values
     */
    private String scenario = String.valueOf(System.currentTimeMillis());
    private PerformanceMeasures.Measure measure = PerformanceMeasures.Measure.ACCURACY;
    private boolean startWithDefault = true;

    /**
     * Instantiates a enw randomSearch builder
     */
    public RandomSearchBuilder() {

    }

    /**
     * Instantiates a enw randomSearch builder
     *
     * @param explainedInstanceIndex the index of the explained instance
     * @param anchorTabular          the anchor tabular of the dataset
     * @param classificationFunction the classification function used
     * @param configurationSpace     the full configuration space to optimize
     * @param executionTermination   the termination condition in number of executions
     */
    private RandomSearchBuilder(int[] explainedInstanceIndex,
                                AnchorTabular anchorTabular,
                                Function<TabularInstance, Integer> classificationFunction,
                                ConfigurationSpace configurationSpace,
                                int executionTermination) {
        this.explainedInstanceIndex = explainedInstanceIndex;
        this.anchorTabular = anchorTabular;
        this.classificationFunction = classificationFunction;
        this.configurationSpace = configurationSpace;
        this.executionTermination = executionTermination;
    }

    /**
     * Create the Default Constructor
     *
     * @param explainedInstanceIndex the index of the explained instance
     * @param anchorTabular          the anchor tabular of the dataset
     * @param classificationFunction the classification function used
     * @param configurationSpace     the full configuration space to optimize
     * @return the {@link RandomSearchBuilder}
     */
    public RandomSearch createDefaultBuilder(AnchorTabular anchorTabular,
                                             Function<TabularInstance, Integer> classificationFunction,
                                             ConfigurationSpace configurationSpace,
                                             int... explainedInstanceIndex) {
        return new RandomSearchBuilder(explainedInstanceIndex, anchorTabular, classificationFunction, configurationSpace, 30).build();
    }

    /**
     * Sets the classification function
     *
     * @param classificationFunction the classification function
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setClassificationFunction(Function<TabularInstance, Integer> classificationFunction) {
        this.classificationFunction = classificationFunction;

        return this;
    }


    /**
     * Sets the scenario
     *
     * @param scenario the scenario name
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setScenario(String scenario) {
        this.scenario = scenario;

        return this;
    }

    /**
     * Sets the time termination condition
     *
     * @param seconds the termination condition in seconds
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setTimeTermination(long seconds) {
        this.timeTermination = seconds;

        return this;
    }

    /**
     * Sets the number of executions as termination condition
     *
     * @param executionCount the termination condition in number of executions
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setExecutionTermination(int executionCount) {
        this.executionTermination = executionCount;

        return this;
    }

    /**
     * Sets the configuration space to optimize
     *
     * @param configurationSpace the full configuration space to optimize
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setConfigurationSpace(ConfigurationSpace configurationSpace) {
        this.configurationSpace = configurationSpace;

        return this;
    }

    /**
     * Optimization process will not start with the default configuration
     *
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder notStartWithDefault() {
        this.startWithDefault = false;

        return this;
    }

    /**
     * Sets the performance measure to optimize on
     * <p>
     * If not set ACCURACY will be used as performance measurement
     *
     * @param performanceMeasure the performance measure it will be optimized on
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setMeasure(PerformanceMeasures.Measure performanceMeasure) {
        this.measure = performanceMeasure;

        return this;
    }

    /**
     * Sets the index of the instance that is to be explained
     * <p>
     * If not set the used index is 0
     *
     * @param explainedInstanceIndex the index of the explained instance
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setExplainedInstanceIndex(int... explainedInstanceIndex) {
        this.explainedInstanceIndex = explainedInstanceIndex;

        return this;
    }


    public RandomSearchBuilder setData(DataInitializer data) {
        this.data = data;

        return this;
    }


    /**
     * Build the Random Search instance setting all values or their pre-configures default values.
     *
     * @return the Random Search instance
     */
    public RandomSearch build() {
        return new RandomSearch(scenario, explainedInstanceIndex, data, configurationSpace, timeTermination, executionTermination, startWithDefault, classificationFunction, measure);
    }


}
