package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Discretizer partitioning data into n specified classes using their mean values as a class label
 */
public class PercentileMedianDiscretizer implements Discretizer {
    private static final long serialVersionUID = -5389805012441004957L;

    private final int classCount;
    private List<DiscretizerRelation> discretizerRelations;

    /**
     * Creates the discretizer.
     *
     * @param classCount the amount of classes to use
     */
    public PercentileMedianDiscretizer(int classCount) {
        this.classCount = classCount;
    }

    private static double medianIndexValue(List<Number> list) {
        if (list.size() % 2 == 0) {
            return (list.get(list.size() / 2).doubleValue() + list.get(list.size() / 2 - 1).doubleValue()) / 2;
        } else {
            return list.get(list.size() / 2).doubleValue();
        }
    }

    @Override
    public void fit(Serializable[] values) {
        final List<Number> numbers = Stream.of(values).map(i -> (Number) i)
                .sorted(Comparator.comparingDouble(Number::doubleValue))
                .collect(Collectors.toList());
        discretizerRelations = new ArrayList<>(this.classCount);

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
            final int medianValue = (int) medianIndexValue(sublist);
            discretizerRelations.add(new DiscretizerRelation(medianValue,
                    sublist.get(0).doubleValue(),
                    sublist.get(sublist.size() - 1).doubleValue()));

        }
    }

    @Override
    public DiscretizerRelation unFit(int value) {
        return this.discretizerRelations.stream().filter((relation) -> Objects.equals(relation.getDiscretizedValue(), value))
                .findFirst().orElseThrow(() -> new IllegalArgumentException("Discrete value " + value + " not in discretized bounds"));
    }

    @Override
    public Integer apply(Serializable o) {
        double value = (double) o;
        return this.discretizerRelations.stream()
                .filter((relation) -> value >= relation.getConditionMin() && value <= relation.getConditionMax())
                .findFirst().orElseThrow(() -> new IllegalArgumentException("Value " + o + " not in discretizer bounds"))
                .getDiscretizedValue();
    }
}
