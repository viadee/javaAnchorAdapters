package de.viadee.anchorj.tabular;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.Set;

import de.viadee.anchorj.PerturbationFunction;
import de.viadee.anchorj.global.ReconfigurablePerturbationFunction;

/**
 * Implementation of the {@link PerturbationFunction} for arbitrary de.viadee.anchorj.tabular datasets.
 * <p>
 * Uses a set of {@link TabularInstance}s to randomly return, e.g. the testing set.
 * <p>
 * Creating a custom de.viadee.anchorj.tabular perturbation function is highly application specific.
 */
public class TabularPerturbationFunction implements ReconfigurablePerturbationFunction<TabularInstance> {
    private static final long serialVersionUID = 5592705226886781336L;

    private final TabularInstance instance;
    private final TabularInstance[] perturbationData;

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
    }


    @Override
    public PerturbationFunction<TabularInstance> createForInstance(TabularInstance instance) {
        return new TabularPerturbationFunction(instance, this.perturbationData, this.seed);
    }

    @Override
    public PerturbationFunction.PerturbationResult<TabularInstance> perturb(Set<Integer> immutableFeaturesIdx,
                                                                            int noPerturbations) {
        // Extend list until space is large enough
        List<TabularInstance> shuffledPerturbations = new ArrayList<>();
        while (shuffledPerturbations.size() < noPerturbations)
            shuffledPerturbations.addAll(Arrays.asList(this.perturbationData));

        // TODO instead of shuffle rather use ThreadLocalRandom?
        if (this.random == null) {
            Collections.shuffle(shuffledPerturbations);
        } else {
            Collections.shuffle(shuffledPerturbations, this.random);
        }

        List<TabularInstance> rawResult = new ArrayList<>();
        List<boolean[]> featuresChanged = new ArrayList<>();
        for (int i = 0; i < noPerturbations; i++) {
            final TabularInstance instanceToClone = shuffledPerturbations.get(i);
            final TabularInstance perturbedInstance = new TabularInstance(instanceToClone);
            // Copy all fixed features
            for (Integer featureId : immutableFeaturesIdx) {
                perturbedInstance.getInstance()[featureId] = instance.getInstance()[featureId];
                perturbedInstance.getTransformedInstance()[featureId] = instance.getTransformedInstance()[featureId];
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
