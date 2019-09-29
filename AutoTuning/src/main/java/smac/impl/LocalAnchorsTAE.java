package smac.impl;

import dataInitialization.DataInitializer;
import randomSearch.Logger;
import smac.util.AnchorsConfig;
import ca.ubc.cs.beta.aeatk.algorithmrunconfiguration.AlgorithmRunConfiguration;
import ca.ubc.cs.beta.aeatk.algorithmrunresult.AlgorithmRunResult;
import ca.ubc.cs.beta.aeatk.algorithmrunresult.ExistingAlgorithmRunResult;
import ca.ubc.cs.beta.aeatk.algorithmrunresult.RunStatus;
import ca.ubc.cs.beta.aeatk.parameterconfigurationspace.ParameterConfiguration;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluator;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluatorCallback;
import ca.ubc.cs.beta.aeatk.targetalgorithmevaluator.TargetAlgorithmEvaluatorRunObserver;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.TabularPerturbationFunction;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.execution.sampling.DefaultSamplingFunction;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

public class LocalAnchorsTAE implements TargetAlgorithmEvaluator {

    private final Function<TabularInstance, Integer> classificationFunction;
    private final DataInitializer data;
    private final Logger logger;
    private final int explainedInstanceIndex;

    int time = 0;
    private AnchorTabular anchorTabular;

    public LocalAnchorsTAE(int explainedInstanceIndex, Function<TabularInstance, Integer> classificationFunction, DataInitializer data, Logger logger) {
        this.explainedInstanceIndex = explainedInstanceIndex;
        this.classificationFunction = classificationFunction;
        this.data = data;
        this.logger = logger;
        this.anchorTabular = data.createTabular(null);
    }

    @Override
    public List<AlgorithmRunResult> evaluateRun(AlgorithmRunConfiguration algorithmRunConfiguration) {
        return this.evaluateRun(Collections.singletonList(algorithmRunConfiguration), null);
    }

    @Override
    public List<AlgorithmRunResult> evaluateRun(List<AlgorithmRunConfiguration> list) {
        return this.evaluateRun(list, null);
    }

    @Override
    public List<AlgorithmRunResult> evaluateRun(List<AlgorithmRunConfiguration> list, TargetAlgorithmEvaluatorRunObserver targetAlgorithmEvaluatorRunObserver) {

        List<AlgorithmRunResult> ar = new ArrayList<>();

        for (AlgorithmRunConfiguration ac : list) {

            ParameterConfiguration configuration = ac.getParameterConfiguration();

            long runtimeStart = System.currentTimeMillis();

            anchorTabular = data.createTabular(AnchorsConfig.setDiscretizerForSmac(configuration, anchorTabular));

            final TabularInstance explainedInstance = anchorTabular.getTabularInstances()[explainedInstanceIndex];
            final TabularPerturbationFunction perturbationFunction = new TabularPerturbationFunction(explainedInstance, anchorTabular.getTabularInstances());
            final Integer explainedInstanceLabel = classificationFunction.apply(explainedInstance);

            double emptyRulePrecision = getEmptyRulePrecision(perturbationFunction, explainedInstanceLabel);
            AnchorConstructionBuilder anchorBuilder = setAnchorConstructionBuilder(perturbationFunction, explainedInstanceLabel, explainedInstance);

            AnchorResult<TabularInstance> result = AnchorsConfig.setParametersForSmac(configuration, anchorBuilder).build().constructAnchor();

            final double runtime = System.currentTimeMillis() - runtimeStart;
            final double coverage = result.getCoverage();
            final double addedPrec = result.getPrecision() - emptyRulePrecision;
            final double quality = 1 - (coverage * addedPrec);

            // log results
            logger.addSMACValues(configuration);
            logger.addValuesToLogging(runtime, coverage, addedPrec);
            logger.addRulesToLogging(anchorTabular.getVisualizer().visualizeResult(result));
            logger.endLine();

            ar.add(new ExistingAlgorithmRunResult(ac, RunStatus.SAT, runtime, 1, quality , 10L, anchorTabular.getVisualizer().visualizeResult(result)));
        }
        return ar;
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

    @Override
    public void evaluateRunsAsync(AlgorithmRunConfiguration algorithmRunConfiguration, TargetAlgorithmEvaluatorCallback targetAlgorithmEvaluatorCallback) {

    }

    @Override
    public void evaluateRunsAsync(List<AlgorithmRunConfiguration> list, TargetAlgorithmEvaluatorCallback targetAlgorithmEvaluatorCallback) {

    }

    @Override
    public void evaluateRunsAsync(List<AlgorithmRunConfiguration> list, TargetAlgorithmEvaluatorCallback targetAlgorithmEvaluatorCallback, TargetAlgorithmEvaluatorRunObserver targetAlgorithmEvaluatorRunObserver) {

    }

    @Override
    public void waitForOutstandingEvaluations() {

    }

    @Override
    public int getNumberOfOutstandingEvaluations() {
        return 0;
    }

    @Override
    public int getNumberOfOutstandingBatches() {
        return 0;
    }

    @Override
    public int getNumberOfOutstandingRuns() {
        return 0;
    }

    @Override
    public int getRunCount() {
        return 0;
    }

    @Override
    public int getRunHash() {
        return 0;
    }

    @Override
    public void seek(List<AlgorithmRunResult> list) {

    }

    @Override
    public String getManualCallString(AlgorithmRunConfiguration algorithmRunConfiguration) {
        return null;
    }

    @Override
    public void notifyShutdown() {

    }

    @Override
    public boolean isRunFinal() {
        return false;
    }

    @Override
    public boolean areRunsPersisted() {
        return false;
    }

    @Override
    public boolean areRunsObservable() {
        return false;
    }

    @Override
    public void close() {

    }
}
