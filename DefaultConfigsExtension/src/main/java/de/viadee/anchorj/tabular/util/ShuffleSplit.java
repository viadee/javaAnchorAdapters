package de.viadee.anchorj.tabular.util;

import de.viadee.anchorj.tabular.TabularInstance;
import de.viadee.anchorj.util.ParameterValidation;

import java.util.*;

/**
 * Provides methods to shuffle and split a dataset in order to create variable sized subsets
 * for e.g. training, testing and validation
 */
public enum ShuffleSplit {;

    /**
     * Shuffles and splits this list into two.
     * <p>
     * Useful e.g. if splitting test and validation lists
     *
     * @param tabularInstances the instances to be shuffled
     * @param percentageSize   the percentage of entries the first list will contain
     * @return a multidimensional array with two x indices - one for each split list
     */
    public static TabularInstance[][] shuffleSplit(final TabularInstance[] tabularInstances,
                                                   final double percentageSize) {
        return shuffleSplit(tabularInstances, percentageSize, new Random());
    }

    /**
     * Shuffles and splits this list into two.
     * <p>
     * Useful e.g. if splitting test and validation lists
     *
     * @param tabularInstances the instances to be shuffled
     * @param percentageSize   the percentage of entries the first list will contain
     * @param rnd              the Random to be used
     * @return a multidimensional array with two x indices - one for each split list
     */
    public static TabularInstance[][] shuffleSplit(final TabularInstance[] tabularInstances,
                                                   final double percentageSize, final Random rnd) {
        if (!ParameterValidation.isPercentage(percentageSize))
            throw new IllegalArgumentException("Percentage size" + ParameterValidation.NOT_PERCENTAGE_MESSAGE);

        if (tabularInstances.length == 0)
            return new TabularInstance[][]{new TabularInstance[0], new TabularInstance[0]};

        if (percentageSize == 1)
            return new TabularInstance[][]{tabularInstances, new TabularInstance[0]};
        if (percentageSize == 0)
            return new TabularInstance[][]{new TabularInstance[0], tabularInstances};

        List<TabularInstance> instances = new ArrayList<>(Arrays.asList(tabularInstances));
        Collections.shuffle(instances, rnd);

        final int splitIndex = instances.size() - (int) (instances.size() * percentageSize) - 1;
        final List<TabularInstance> firstList = instances.subList(0, splitIndex);
        final List<TabularInstance> secondList = instances.subList(splitIndex, instances.size());

        return new TabularInstance[][]{
                firstList.toArray(new TabularInstance[0]),
                secondList.toArray(new TabularInstance[0])};
    }
}
