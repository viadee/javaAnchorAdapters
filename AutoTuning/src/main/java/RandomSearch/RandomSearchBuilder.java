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
     */
    public RandomSearchBuilder() {

    }

    /**
     * Instantiates a enw RandomSearch builder
     *
     * @param anchorBuilder          the anchor builder
     * @param anchorTabular          the anchor tabular of the dataset
     * @param classificationFunction the classification function used
     * @param configurationSpace     the full configuration space to optimize
     * @param executionTermination   the termination condition in number of executions
     */
    private RandomSearchBuilder(AnchorConstructionBuilder<TabularInstance> anchorBuilder,
                                AnchorTabular anchorTabular,
                                Function<TabularInstance, Integer> classificationFunction,
                                ConfigurationSpace configurationSpace,
                                int executionTermination) {
        this.anchorBuilder = anchorBuilder;
        this.anchorTabular = anchorTabular;
        this.classificationFunction = classificationFunction;
        this.configurationSpace = configurationSpace;
        this.executionTermination = executionTermination;
    }

    /**
     * Create the Default Constructor
     *
     * @param anchorBuilder          the anchor builder
     * @param anchorTabular          the anchor tabular of the dataset
     * @param classificationFunction the classification function used
     * @param configurationSpace     the full configuration space to optimize
     * @return the {@link RandomSearchBuilder}
     */
    public RandomSearch createDefaultBuilder(AnchorTabular anchorTabular,
                                                    AnchorConstructionBuilder<TabularInstance> anchorBuilder,
                                                    Function<TabularInstance, Integer> classificationFunction,
                                                    ConfigurationSpace configurationSpace) {
        return new RandomSearchBuilder(anchorBuilder, anchorTabular,classificationFunction,configurationSpace, 30).build();
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

    /**
     * Build the Random Search instance setting all values or their pre-configures default values.
     *
     * @return the Random Search instance
     */
    public RandomSearch build() {
        return new RandomSearch(scenario, anchorBuilder, anchorTabular, configurationSpace, timeTermination, executionTermination, startWithDefault, classificationFunction, measure);
    }


}
