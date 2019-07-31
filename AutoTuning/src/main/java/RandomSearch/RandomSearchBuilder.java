package RandomSearch;

import LossFunctions.PerformanceMeasures;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;

import java.util.function.Function;

/**
 * The builder class for the random search optimization method for the Anchors explanations.
 * <p></p>
 * Set various needed values to build an instance of the RandomSearch class.
 */
public class RandomSearchBuilder {

    private AnchorConstructionBuilder<TabularInstance> anchorBuilder;
    private AnchorTabular anchorTabular;
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
     * Instantiates a enw RandomSearch builder
     *
     * @param scenario               the given scenario
     * @param anchorBuilder          the anchor builder
     * @param anchorTabular          the anchor tabular of the dataset
     * @param classificationFunction the classification function used
     * @param configurationSpace     the full configuration space to optimize
     * @param timeTermination        the termination condition in seconds
     * @param executionTermination   the termination condition in number of executions
     * @param measure                the measure to optimize on
     * @param startWithDefault       should optimization be started with the default value
     */
    private RandomSearchBuilder(String scenario,
                                AnchorConstructionBuilder<TabularInstance> anchorBuilder,
                                AnchorTabular anchorTabular,
                                Function<TabularInstance, Integer> classificationFunction,
                                ConfigurationSpace configurationSpace,
                                long timeTermination,
                                int executionTermination,
                                PerformanceMeasures.Measure measure,
                                boolean startWithDefault) {
        this.scenario = scenario;
        this.anchorBuilder = anchorBuilder;
        this.anchorTabular = anchorTabular;
        this.classificationFunction = classificationFunction;
        this.configurationSpace = configurationSpace;
        this.timeTermination = timeTermination;
        this.executionTermination = executionTermination;
        this.measure = measure;
        this.startWithDefault = startWithDefault;
    }

    public RandomSearchBuilder() {

    }

    /**
     * Sets the anchor Builder
     *
     * @param anchorBuilder the anchor builder
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setAnchorBuilder(AnchorConstructionBuilder<TabularInstance> anchorBuilder) {
        this.anchorBuilder = anchorBuilder;

        return this;
    }

    /**
     * Sets the anchor tabular
     *
     * @param anchorTabular the anchor tabular of given dataset
     * @return the current {@link RandomSearchBuilder} for chaining
     */
    public RandomSearchBuilder setAnchorTabular(AnchorTabular anchorTabular) {
        this.anchorTabular = anchorTabular;

        return this;
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

    private void prepareForBuild() {
        if (this.configurationSpace == null)
            this.configurationSpace = new ConfigurationSpace();
    }

    /**
     * Build the Random Search instance setting all values or their pre-configures default values.
     *
     * @return the Random Search instance
     */
    public RandomSearch build() {
        prepareForBuild();
        return new RandomSearch(scenario, anchorBuilder, anchorTabular, configurationSpace, timeTermination, executionTermination, startWithDefault, classificationFunction, measure);
    }


}
