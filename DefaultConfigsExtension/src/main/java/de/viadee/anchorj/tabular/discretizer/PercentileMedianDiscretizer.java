package de.viadee.anchorj.tabular.discretizer;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Discretizer partitioning data into n specified classes using their mean values as a class label
 */
public class PercentileMedianDiscretizer implements Discretizer {
    private final int classCount;
    private Map<Number, Integer> medianValueMapping;

    /**
     * Creates the discretizer.
     *
     * @param classCount the amount of classes to use
     */
    public PercentileMedianDiscretizer(int classCount) {
        this.classCount = classCount;
    }

    private static double medianIndexValue(List<Number> list) {
        if (list.size() % 2 == 0)
            return (list.get(list.size() / 2).doubleValue() + list.get(list.size() / 2 - 1).doubleValue()) / 2;
        else
            return list.get(list.size() / 2).doubleValue();
    }

    @Override
    public void fit(Object[] values) {
        final List<Number> numbers = Stream.of(values).map(i -> (Number) i)
                .sorted(Comparator.comparingDouble(Number::doubleValue))
                .collect(Collectors.toList());
        medianValueMapping = new HashMap<>();
        final int classes = Math.min(classCount, numbers.size());
        final int countPerClass = numbers.size() / classes;
        int backlog = numbers.size() % classes;
        int endIndex = 0;
        for (int currentClass = 0; currentClass < classes; currentClass++) {
            final int startIndex = endIndex;
            endIndex = startIndex + countPerClass;
            if (backlog > 0) {
                endIndex++;
                backlog--;
            }
            List<Number> sublist = numbers.subList(startIndex, endIndex);
            final double medianValue = medianIndexValue(sublist);
            for (Number num : sublist) {
                if (!medianValueMapping.containsKey(num))
                    medianValueMapping.put(num, (int) medianValue);
            }

        }
    }

    @Override
    public Integer apply(Object o) {
        return medianValueMapping.get(o);
    }
}
