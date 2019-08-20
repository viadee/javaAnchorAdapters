package de.viadee.xai.anchor.adapter.tabular;

import de.viadee.xai.anchor.algorithm.PerturbationFunction;
import de.viadee.xai.anchor.algorithm.global.ReconfigurablePerturbationFunction;

import java.util.*;
import java.util.stream.Stream;

/**
 * Implementation of the {@link PerturbationFunction} for arbitrary
 * de.viadee.xai.anchor.adapter.tabular datasets.
 * <p>
 * Uses a set of {@link TabularInstance}s to randomly return, e.g. the testing set.
 * <p>
 * Creating a custom de.viadee.xai.anchor.adapter.tabular perturbation function is highly application specific.
 * <p>
 * Quote Ribeiro: "We use a validation dataset to define D, and sample from D(z|A) by fixing the predicates in A and
 * sampling the rest of the row as a whole."
 */
public class TabularPerturbationFunction implements ReconfigurablePerturbationFunction<TabularInstance> {
    private static final long serialVersionUID = 5592705226886781336L;

    private final TabularInstance instance;
    private final TabularInstance[] perturbationData;

    private final TabularInstance[][] reverseIndex;

    private Long seed;
    private Random random;

    /**
     * Constructs the instance.
     *
     * @param instance         the {@link TabularInstance} to perturb.
     * @param perturbationData an array of data to generate perturbations from
     */
    public TabularPerturbationFunction(TabularInstance instance, TabularInstance[] perturbationData) {
        this(instance, perturbationData, null);
    }

    /**
     * Constructs the instance.
     *
     * @param instance         the {@link TabularInstance} to perturb.
     * @param perturbationData an array of data to generate perturbations from
     * @param seed             the seed to use
     */
    public TabularPerturbationFunction(TabularInstance instance, TabularInstance[] perturbationData, Long seed) {
        if (perturbationData == null || perturbationData.length < 1)
            throw new IllegalArgumentException("Perturbation data must have at least one element");
        this.instance = instance;
        this.perturbationData = perturbationData;
        if (seed != null) {
            this.seed = seed;
            this.random = new Random(this.seed);
        }
        reverseIndex = createReverseIndex(instance, perturbationData);
    }

    /**
     * @param instance         the explained instance
     * @param perturbationData the perturbation data to sample new values from
     * @return a 2D array containing for each index an array with all instances that match the discretized values.
     * These instances can be used to draw transformed values from and thus to change a fixed feature's transformed value
     */
    private static TabularInstance[][] createReverseIndex(TabularInstance instance,
                                                          TabularInstance[] perturbationData) {
        final TabularInstance[][] result = new TabularInstance[instance.getInstance().length][];
        for (int i = 0; i < instance.getInstance().length; i++) {
            final int index = i;
            final Double discretizedColumnValue = instance.getInstance()[index];
            result[i] = Stream.of(perturbationData)
                    .filter(pert -> discretizedColumnValue.equals(pert.getInstance()[index]))
                    .toArray(TabularInstance[]::new);
        }
        return result;
    }


    @Override
    public PerturbationFunction<TabularInstance> createForInstance(TabularInstance instance) {
        return new TabularPerturbationFunction(instance, this.perturbationData, this.seed);
    }

    @Override
    public PerturbationFunction.PerturbationResult<TabularInstance> perturb(Set<Integer> immutableFeaturesIdx,
                                                                            int nrPerturbations) {
        // Extend list until space is large enough
        List<TabularInstance> shuffledPerturbations = new ArrayList<>();
        while (shuffledPerturbations.size() < nrPerturbations)
            shuffledPerturbations.addAll(Arrays.asList(this.perturbationData));

        // TODO instead of shuffle rather use ThreadLocalRandom?
        final Random rnd = (this.random == null)
                ? new Random()
                : this.random;

        Collections.shuffle(shuffledPerturbations, rnd);

        List<TabularInstance> rawResult = new ArrayList<>();
        List<boolean[]> featuresChanged = new ArrayList<>();
        for (int i = 0; i < nrPerturbations; i++) {
            final TabularInstance instanceToClone = shuffledPerturbations.get(i);
            final TabularInstance perturbedInstance = new TabularInstance(instanceToClone);
            // Copy all fixed features. If discretization is used, mutate transformed value within its class
            for (Integer featureId : immutableFeaturesIdx) {
                perturbedInstance.getInstance()[featureId] = instance.getInstance()[featureId];
                final TabularInstance[] sampleFrom = this.reverseIndex[featureId];
                if (sampleFrom.length < 1) {
                    perturbedInstance.getTransformedInstance()[featureId] = instance.getTransformedInstance()[featureId];
                } else {
                    perturbedInstance.getTransformedInstance()[featureId] = sampleFrom[rnd.nextInt(sampleFrom.length)]
                            .getTransformedValue(featureId);
                }
            }
            rawResult.add(perturbedInstance);
            boolean[] tempFeatureChanged = new boolean[perturbedInstance.getFeatureCount()];
            for (int j = 0; j < tempFeatureChanged.length; j++) {
                tempFeatureChanged[j] = !Objects.equals(instance.getInstance()[j], perturbedInstance.getInstance()[j]);
            }
            featuresChanged.add(tempFeatureChanged);
        }

        return new PerturbationResultImpl<>(rawResult.toArray(new TabularInstance[0]),
                featuresChanged.toArray(new boolean[0][]));
    }

}
