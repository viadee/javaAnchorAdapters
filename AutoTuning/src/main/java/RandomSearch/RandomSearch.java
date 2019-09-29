package RandomSearch;

import DataInitialization.DataInitializer;
import LossFunctions.PerformanceMeasures;
import LossFunctions.PredictionModel;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.TabularPerturbationFunction;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.UniqueValueDiscretizer;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.execution.sampling.DefaultSamplingFunction;
import de.viadee.xai.anchor.algorithm.global.CoveragePick;
import de.viadee.xai.anchor.algorithm.util.ParameterValidation;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Class for optimizing a given configuration space for Anchor construction
 */
public class RandomSearch {

    private final String scenario;
    private final int[] explainedInstanceIndex;
    private final Function<TabularInstance, Integer> classificationFunction;
    private final long terminationConditionInSec;
    private final int terminationConditionNrEx;
    private final DataInitializer data;
    private PerformanceMeasures.Measure measure;
    private AnchorTabular anchorTabular;
    private ConfigurationSpace currentConfigurationSpace;
    private ConfigurationSpace incumbent;
    private List<AnchorResult<TabularInstance>> bestExplanations;
    private Logger logger;


    RandomSearch(String scenario,
                 int[] explainedInstanceIndex,
                 DataInitializer data,
                 ConfigurationSpace configurationSpace,
                 long terminationConditionInSec,
                 int terminationConditionNrEx,
                 boolean startWithDefault,
                 Function<TabularInstance, Integer> classificationFunction,
                 PerformanceMeasures.Measure measure) {

        if (configurationSpace == null)
            throw new IllegalArgumentException("Configuration space " + ParameterValidation.NULL_MESSAGE);
        if (data == null)
            throw new IllegalArgumentException("Data " + ParameterValidation.NULL_MESSAGE);
        if (classificationFunction == null)
            throw new IllegalArgumentException("Classification function " + ParameterValidation.NULL_MESSAGE);
        if (terminationConditionInSec == 0 && terminationConditionNrEx == 0)
            throw new IllegalArgumentException("No termination condition defined to run random search");
        if (explainedInstanceIndex.length == 0)
            throw new IllegalArgumentException("Instance " + ParameterValidation.NULL_MESSAGE);

        this.scenario = scenario;
        this.explainedInstanceIndex = explainedInstanceIndex;
        this.data = data;
        this.currentConfigurationSpace = configurationSpace;
        this.classificationFunction = classificationFunction;
        this.measure = measure;
        this.terminationConditionInSec = terminationConditionInSec;
        this.terminationConditionNrEx = terminationConditionNrEx;

        this.incumbent = new ConfigurationSpace(configurationSpace);
        this.anchorTabular = data.createTabular(null);
        this.logger = new Logger(scenario, currentConfigurationSpace, measure);
    }

    /**
     *
     */
    public void optimizeLocalExplanations() {
        measure = PerformanceMeasures.Measure.PRECISION;
        this.optimizeExplanations(false);
    }

    /**
     *
     */
    public void optimizeGlobalExplanations() {
        this.optimizeExplanations(true);
    }


    /**
     * The construction of anchors for all instances in the dataset and the collection of global rules from these local rules via the Coverage Pick.
     *
     * @param anchorBuilder the anchorBuilder that is used to construct the anchors.
     * @return the global explanations from the Coverage Pick.
     */
    private List<AnchorResult<TabularInstance>> optimizeGlobal(AnchorConstructionBuilder<TabularInstance> anchorBuilder) {

        TabularInstance[] instances = new TabularInstance[explainedInstanceIndex.length];
        for (int i = 0; i < instances.length; i++) {
            instances[i] = anchorTabular.getTabularInstances()[explainedInstanceIndex[i]];
        }

        return new CoveragePick<>(anchorBuilder, 10,
                Executors.newCachedThreadPool(), null)
                .run(instances, 20);
    }

    /**
     * The construction of the local anchor for a single instance.
     *
     * @param anchorBuilder the anchorBuilder that is used to construct an anchor.
     * @return the local explanation in a list
     */
    private List<AnchorResult<TabularInstance>> optimizeLocal(AnchorConstructionBuilder<TabularInstance> anchorBuilder) {
        final AnchorResult<TabularInstance> localExplanation = anchorBuilder.build().constructAnchor();
        return Collections.singletonList((localExplanation));
    }

    /**
     * Optimize the explanations given by Anchors either for a certain local instance or for the global explanations created
     * with the coverage pick. The output is a set of optimal parameters, their performance and the resulting rule/s
     *
     * @param global should the optimization be on a global level
     */
    private void optimizeExplanations(boolean global) {

        long startTime = System.currentTimeMillis();
        int nrExecutions = 0;

        while ((System.currentTimeMillis() - startTime) < (terminationConditionInSec * 1000) || nrExecutions < this.terminationConditionNrEx) {

            // to calculate the runtime of each Anchors run
            long runtimeStart = System.currentTimeMillis();

            setNewDiscretizers();

            final TabularInstance explainedInstance = anchorTabular.getTabularInstances()[explainedInstanceIndex[0]];
            final TabularPerturbationFunction perturbationFunction = new TabularPerturbationFunction(explainedInstance, anchorTabular.getTabularInstances());
            final Integer explainedInstanceLabel = classificationFunction.apply(explainedInstance);
            double emptyRulePrecision = getEmptyRulePrecision(perturbationFunction, explainedInstanceLabel);
            AnchorConstructionBuilder anchorBuilder = setAnchorConstructionBuilder(perturbationFunction, explainedInstanceLabel, explainedInstance);

            setNewParameters(anchorBuilder, logger);

            List<AnchorResult<TabularInstance>> rules = global ? optimizeGlobal(anchorBuilder) : optimizeLocal(anchorBuilder);

            // set runtime of current Anchors run
            currentConfigurationSpace.setRuntime(System.currentTimeMillis() - runtimeStart);

            if (global)
                setGlobalPerformance(rules);
            else
                setLocalPerformance(rules, emptyRulePrecision);

            // log results
            logger.addValuesToLogging(currentConfigurationSpace.getRuntime(), currentConfigurationSpace.getCoverage(), currentConfigurationSpace.getPerformance());
            for (AnchorResult rule : rules)
                logger.addRulesToLogging(anchorTabular.getVisualizer().visualizeResult(rule));
            logger.endLine();

            // check if performance of current space is the best, if yes set current space as best space
            if (checkIfBetter(currentConfigurationSpace.getPerformance(), currentConfigurationSpace.getCoverage())) {
                incumbent = new ConfigurationSpace(currentConfigurationSpace);
                bestExplanations = rules;
            }
            nrExecutions++;
        }

        //randomSearchLogger.addRulesToLogging(anchorTabular.getVisualizer().visualizeGlobalResults(bestExplanations));
        logger.endLogging();

        // visualize best hyperparameters and the best global explanations
        visualizeBestHyperparameterSpace();
        System.out.println(anchorTabular.getVisualizer().visualizeGlobalResults(bestExplanations));
    }

    /**
     * Create the AnchorsConstructionBuilder with the perturbation function, the label of the explained instance and the explained instance self. Set the emptyRuleEvaluation to 0 to exclude the empty
     * rule from being selected as anchor.
     *
     * @param perturbationFunction   the perturbation function
     * @param explainedInstanceLabel the label of the explained instance
     * @param explainedInstance      the explained instance
     * @return the AnchorConstructionBuilder
     */
    private AnchorConstructionBuilder setAnchorConstructionBuilder(TabularPerturbationFunction perturbationFunction, final Integer explainedInstanceLabel, TabularInstance explainedInstance) {
        return new AnchorConstructionBuilder<>(
                new DefaultSamplingFunction<>(classificationFunction::apply, perturbationFunction),
                explainedInstance, explainedInstanceLabel)
                .setEmptyRuleEvaluations(0);
    }

    /**
     * Get the precision of the empty rule for a single instance. This empty rule precision is important to calculate the added-precision instead of the full precision to
     * evaluate the improvement over the base precision of the given label. The number of evaluations define how accurate this precision is, because it is calculated
     * with random perturbations from the data set. A higher number of perturbations is more likely to represent the full data set.
     *
     * @param perturbationFunction   the perturbation function
     * @param explainedInstanceLabel the label of the explained instance
     * @return the precision of the empty rule for a single instance label
     */
    private double getEmptyRulePrecision(TabularPerturbationFunction perturbationFunction, final Integer explainedInstanceLabel) {
        final int perturbationCount = 1000;
        return Stream.of(perturbationFunction.perturb(Collections.emptySet(), perturbationCount).getRawResult())
                .map(classificationFunction)
                .filter(explainedInstanceLabel::equals)
                .count() / (double) perturbationCount;
    }

    /**
     * Set new randomly chosen discretizers for each column that was chosen to be discretized (that is not discretized via the UniqueValueDiscretizer).
     */
    private void setNewDiscretizers() {
        DiscretizationSpace ds = currentConfigurationSpace.getDiscretizationSpace();
        Map<String, Discretizer> discretizerMap = new HashMap<>();

        if (ds != null) {
            for (GenericColumn column : anchorTabular.getColumns()) {
                if (column.getDiscretizer().getClass() != UniqueValueDiscretizer.class)
                    discretizerMap.put(column.getName(), ds.getRandomDiscretizer());
            }
        }
        logger.addDiscretizerValue(discretizerMap);
        this.anchorTabular = data.createTabular(discretizerMap);
    }

    /**
     * Set all parameters that the Anchors algorithm needs randomly new.
     *
     * @param anchorBuilder the anchorBuilder where the parameters are changed
     */
    private void setNewParameters(AnchorConstructionBuilder<TabularInstance> anchorBuilder, Logger logger) {

        Map<String, String> parameterValues = currentConfigurationSpace.getHyperparameterSpace().randomizeParameters();

        if (parameterValues.containsKey("tau"))
            anchorBuilder.setTau(Double.valueOf(parameterValues.get("tau")));
        if (parameterValues.containsKey("beamsize"))
            anchorBuilder.setBeamSize(Integer.valueOf(parameterValues.get("beamsize")));
        if (parameterValues.containsKey("delta"))
            anchorBuilder.setDelta(Double.valueOf(parameterValues.get("delta")));
        if (parameterValues.containsKey("epsilon"))
            anchorBuilder.setEpsilon(Double.valueOf(parameterValues.get("epsilon")));
        if (parameterValues.containsKey("tauDiscrepancy"))
            anchorBuilder.setTauDiscrepancy(Double.valueOf(parameterValues.get("tauDiscrepancy")));
        if (parameterValues.containsKey("setInitSampleCount"))
            anchorBuilder.setInitSampleCount(Integer.valueOf(parameterValues.get("setInitSampleCount")));
        if (parameterValues.containsKey("allowSuboptimalSteps"))
            anchorBuilder.setAllowSuboptimalSteps(Boolean.valueOf(parameterValues.get("allowSuboptimalSteps")));

        logger.addParameterValues(parameterValues);
    }

    /**
     * Calculate the performance of the local anchor
     *
     * @param rule               the local rule to be evaluated
     * @param emptyRulePrecision the precision of the empty-rule
     */
    private void setLocalPerformance(List<AnchorResult<TabularInstance>> rule, double emptyRulePrecision) {
        currentConfigurationSpace.setPerformance(rule.get(0).getPrecision() - emptyRulePrecision);
        currentConfigurationSpace.setCoverage(rule.get(0).getCoverage());
    }

    /**
     * Sets the performance and coverage of the global rule set
     *
     * @param rules the rules picked by the Coverage Pick
     */
    private void setGlobalPerformance(List<AnchorResult<TabularInstance>> rules) {
        // predict labels of instances based on generated global rules
        PredictionModel model = new PredictionModel(rules);
        List<Integer> prediction = model.predict(anchorTabular.getTabularInstances());
        PerformanceMeasures performanceMeasures = new PerformanceMeasures(prediction, classificationFunction, anchorTabular.getTabularInstances());
        currentConfigurationSpace.setPerformance(performanceMeasures.calcMeasure(measure));
        currentConfigurationSpace.setCoverage(performanceMeasures.getCoverage());
    }

    /**
     * Checks whether the current configuration performs better than the current incumbent.
     *
     * @param performance precision of the rule/s generated via the current configuration
     * @param coverage    coverage of the rule/s generated via the current configuration
     * @return true if the performance is better than the incumbent, false otherwise
     */
    private boolean checkIfBetter(double performance, double coverage) {
        return (this.incumbent.getPerformance() * this.incumbent.getCoverage() == 0 || performance * coverage > this.incumbent.getPerformance() * this.incumbent.getCoverage());
    }

    /**
     * Visualize the best configuration space upon termination of the optimization algorithm.
     */
    private void visualizeBestHyperparameterSpace() {
        System.out.println("==== incumbent performance ====" + System.lineSeparator() +
                "coverage: " + this.incumbent.getCoverage() + System.lineSeparator() +
                measure.toString().toLowerCase() + ": " + this.incumbent.getPerformance() + System.lineSeparator() +
                "runtime: " + this.incumbent.getRuntime() + "ms");
    }
}
