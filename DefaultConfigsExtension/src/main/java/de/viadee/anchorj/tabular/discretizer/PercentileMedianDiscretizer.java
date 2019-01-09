package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Discretizer partitioning data into n specified classes using their mean values as a class label
 */
public class PercentileMedianDiscretizer implements Discretizer {
    private static final long serialVersionUID = -5389805012441004957L;

    private int classCount;
    private final boolean automaticFitting;
    private final List<DiscretizerRelation> discretizerRelations;
    private final List<Number> singleClassValues;
    private final List<DiscretizerRelation> singleClassValueRelations;

    /**
     * Creates the discretizer with automatic fitting true.
     *
     * @param classCount        the amount of classes to use
     * @param singleClassValues values which each of them should be a class like null values
     */
    public PercentileMedianDiscretizer(int classCount, Number... singleClassValues) {
        this(classCount, true, singleClassValues);
    }

    /**
     * Creates the discretizer.
     *
     * @param classCount        the amount of classes to use
     * @param automaticFitting  if it happens that multiple relations have the same discretized values these relations
     *                          will be combined. If then this combined relation is the last remaining (except for single
     *                          class values) the class count will be increased as long as this condition is true or
     *                          class count is higher than the number of values
     * @param singleClassValues values which each of them should be a class like null values
     */
    public PercentileMedianDiscretizer(int classCount, boolean automaticFitting, Number... singleClassValues) {
        this.classCount = classCount;
        this.automaticFitting = automaticFitting;

        if (singleClassValues == null) {
            singleClassValues = new Number[0];
        }
        discretizerRelations = new ArrayList<>(this.classCount + singleClassValues.length);
        if (singleClassValues.length == 0) {
            this.singleClassValues = Collections.emptyList();
            this.singleClassValueRelations = Collections.emptyList();
        } else {
            this.singleClassValues = new ArrayList<>(singleClassValues.length);
            this.singleClassValueRelations = new ArrayList<>(singleClassValues.length);
            for (Number singleClassValue : singleClassValues) {
                this.singleClassValues.add(singleClassValue.intValue());
                this.singleClassValueRelations.add(new DiscretizerRelation(singleClassValue.intValue(),
                        singleClassValue.doubleValue(), singleClassValue.doubleValue()));
            }
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
        List<Number> numbers = Stream.of(values).map(i -> (Number) i)
                .filter(number -> !singleClassValues.contains(number))
                .sorted(Comparator.comparingDouble(Number::doubleValue))
                .collect(Collectors.toList());

        if (values.length > 0 && numbers.size() <= 0) {
            // all values are single class values
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

        if (this.automaticFitting) {
            removeDuplicateDiscretizedValues(values, numbers);
        }

        distinctMinAndMaxValues();
    }

    private void distinctMinAndMaxValues() {
        for (DiscretizerRelation relation : this.discretizerRelations) {
            if ()
        }
    }

    private void removeDuplicateDiscretizedValues(Serializable[] values, List<Number> numbers) {
        List<Integer> discretizedValues = this.discretizerRelations.stream()
                .map(DiscretizerRelation::getDiscretizedValue)
                .collect(Collectors.toList());
        if (discretizedValues.size() > new HashSet<>(discretizedValues).size()) {
            Map<Integer, Long> valueCount = this.discretizerRelations.stream()
                    .map(DiscretizerRelation::getDiscretizedValue)
                    .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));

            for (Map.Entry<Integer, Long> entry : valueCount.entrySet()) {
                if (entry.getValue() > 1) {
                    List<DiscretizerRelation> discretizerRelationsWithSameCatValue = discretizerRelations.stream().filter((rel) -> rel.getDiscretizedValue() == entry.getKey()).collect(Collectors.toList());
                    double conditionMin = discretizerRelationsWithSameCatValue.stream().map(DiscretizerRelation::getConditionMin).min(Double::compareTo).get();
                    double conditionMax = discretizerRelationsWithSameCatValue.stream().map(DiscretizerRelation::getConditionMax).max(Double::compareTo).get();
                    int discretizedValue = discretizerRelationsWithSameCatValue.get(0).getDiscretizedValue();

                    DiscretizerRelation combinedRelation = new DiscretizerRelation(discretizedValue, conditionMin, conditionMax);
                    this.discretizerRelations.removeAll(discretizerRelationsWithSameCatValue);
                    this.discretizerRelations.add(combinedRelation);
                    this.discretizerRelations.sort(Comparator.comparingDouble(DiscretizerRelation::getDiscretizedValue));
                    while (countRelationsWithoutSingleClassRelations() == 1 && this.classCount < numbers.size()) {
                        this.discretizerRelations.clear();
                        this.classCount++;
                        this.fit(values);
//                        double newMin = numbers.stream().map(Number::doubleValue).filter((number -> Objects.equals(number, combinedRelation.getConditionMin()))).min(Double::compare).get();
//                        combinedRelation.setConditionMax(combinedRelation.getConditionMin());
//                        DiscretizerRelation secondRelation = new DiscretizerRelation(combinedRelation);
//                        secondRelation.setConditionMin(newMin);
//                        this.discretizerRelations.add(secondRelation);
                    }

                }
            }
        }
    }

    private long countRelationsWithoutSingleClassRelations() {
        return this.discretizerRelations.stream().filter((relation) -> !this.singleClassValueRelations.contains(relation)).count();
    }

    @Override
    public DiscretizerRelation unApply(final int discretizedValue) {
        return this.discretizerRelations.stream().filter((relation) -> Objects.equals(relation.getDiscretizedValue(), discretizedValue))
                .findFirst().orElseThrow(() -> new IllegalArgumentException("Discrete discretizedValue " + discretizedValue + " not in discretized bounds"));
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
