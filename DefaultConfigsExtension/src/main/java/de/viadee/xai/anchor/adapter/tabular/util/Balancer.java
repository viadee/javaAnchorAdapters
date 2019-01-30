package de.viadee.xai.anchor.adapter.tabular.util;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Provides methods to balance a dataset,
 * i.e. remove instances so that so that each label has the same amount of instances
 */
public enum Balancer {;

    /**
     * Truncates the set of instances so that each label has the same amount of instances
     *
     * @param instances the instances to balance
     * @return the balanced data list
     */
    public static TabularInstance[] balance(TabularInstance[] instances) {
        return balance(instances, new Random());
    }

    /**
     * Truncates the set of instances so that each label has the same amount of instances
     *
     * @param instances the instances to balance
     * @param rnd       the random instance
     * @return the balanced data list
     */
    public static TabularInstance[] balance(TabularInstance[] instances, Random rnd) {
        // Balancing = for each label have the same amount of entries
        if (Stream.of(instances).map(TabularInstance::getDiscretizedLabel).anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("Labels need to be included in all instances when balancing");
        }
        // Balancing = for each label have the same amount of entries
        // We reattach the labels as balancing shuffles the table, which would lead to our labels being faulty

        final Map<Integer, Integer> countPerLabel = Stream.of(instances)
                .collect(Collectors.groupingBy(TabularInstance::getDiscretizedLabel,
                        Collectors.reducing(0, e -> 1, Integer::sum)));
        final int minCount = countPerLabel.values().stream().min(Comparator.comparing(Integer::valueOf)).orElseThrow(()
                -> new IllegalArgumentException("Input list too small"));

        final List<TabularInstance> result = new ArrayList<>();//new TabularInstance[minCount * countPerLabel.size()];

        final List<TabularInstance> list = new ArrayList<>(Arrays.asList(instances));
        Collections.shuffle(list, rnd);
        final Map<Integer, Integer> addedCount = new HashMap<>();
        for (TabularInstance instance : list) {
            final Integer currentCount = addedCount.getOrDefault(instance.getDiscretizedLabel(), 0);
            if (currentCount < minCount) {
                result.add(instance);
                addedCount.put(instance.getDiscretizedLabel(), currentCount + 1);
            }
        }

        // Shuffle again so that labels are distributed equally
        Collections.shuffle(result, rnd);

        return result.toArray(new TabularInstance[0]);
    }
}
