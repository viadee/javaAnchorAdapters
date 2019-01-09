package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Discretizer partitioning data into n specified classes using their mean values as a class label
 */
public class PercentileMedianDiscretizer implements Discretizer {
    private static final long serialVersionUID = -5389805012441004957L;

    private final int classCount;
    private Set<DiscretizerRelation> discretizerRelations;
    private final List<Number> singleClassValues;

    /**
     * Creates the discretizer.
     *
     * @param classCount        the amount of classes to use
     * @param singleClassValues values which each of them should be a class like null values
     */
    public PercentileMedianDiscretizer(int classCount, Number... singleClassValues) {
        this.classCount = classCount;
        if (singleClassValues == null || singleClassValues.length == 0) {
            this.singleClassValues = Collections.emptyList();
        } else {
            this.singleClassValues = Arrays.asList(singleClassValues);
        }
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
        discretizerRelations = new HashSet<>(this.classCount);
        for (Number singleClassValue : this.singleClassValues) {
            discretizerRelations.add(new DiscretizerRelation(singleClassValue.intValue(),
                    singleClassValue.doubleValue(), singleClassValue.doubleValue()));
        }
        List<Number> numbers = Stream.of(values).map(i -> (Number) i)
                .filter(number -> !singleClassValues.contains(number))
                .sorted(Comparator.comparingDouble(Number::doubleValue))
                .collect(Collectors.toList());

        if (values.length > 0 && numbers.size() <= 0) {
            // all values are null
            return;
        }

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

        removeDuplicateDiscretizedValues();
    }

    private void removeDuplicateDiscretizedValues() {
        List<Integer> discretizedValues = discretizerRelations.stream().map(DiscretizerRelation::getDiscretizedValue).collect(Collectors.toList());
        if (discretizedValues.size() > new HashSet<>(discretizedValues).size()) {
            Map<Integer, Long> valueCount = discretizerRelations.stream().map(DiscretizerRelation::getDiscretizedValue).collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));
            for (Map.Entry<Integer, Long> entry : valueCount.entrySet()) {
                if (entry.getValue() > 1) {
                    List<DiscretizerRelation> discretizerRelationsWithSameCatValue = discretizerRelations.stream().filter((rel) -> rel.getDiscretizedValue() == entry.getKey()).collect(Collectors.toList());
                    double conditionMin = discretizerRelationsWithSameCatValue.stream().map(DiscretizerRelation::getConditionMin).min(Double::compareTo).get();
                    double conditionMax = discretizerRelationsWithSameCatValue.stream().map(DiscretizerRelation::getConditionMax).max(Double::compareTo).get();
                    int discretizedValue = discretizerRelationsWithSameCatValue.get(0).getDiscretizedValue();
                    discretizerRelations.removeAll(discretizerRelationsWithSameCatValue);
                    discretizerRelations.add(new DiscretizerRelation(discretizedValue, conditionMin, conditionMax));

                }
            }
        }
    }

    @Override
    public DiscretizerRelation unApply(int value) {
        return this.discretizerRelations.stream().filter((relation) -> Objects.equals(relation.getDiscretizedValue(), value))
                .findFirst().orElseThrow(() -> new IllegalArgumentException("Discrete value " + value + " not in discretized bounds"));
    }

    @Override
    public Integer apply(Serializable o) {
        Number value = (Number) o;
        for (DiscretizerRelation relation : this.discretizerRelations) {
            if (value.doubleValue() >= relation.getConditionMin() && value.doubleValue() <= relation.getConditionMax()) {
                return relation.getDiscretizedValue();
            }
        }

        throw new IllegalArgumentException("Value " + o + " not in discretizer bounds");
    }
}
