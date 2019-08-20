package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;
import de.viadee.xai.anchor.adapter.tabular.discretizer.NumericDiscretizationOrigin;

import java.io.Serializable;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Discretizer partitioning data into n specified classes using their mean values as a class label
 */
public class PercentileMedianDiscretizer extends AbstractDiscretizer {
    private final boolean classReduction;
    private int classCount;

    /**
     * Constructs the instance
     *
     * @param classCount count of classes
     */
    public PercentileMedianDiscretizer(int classCount) {
        this(classCount, true);
    }

    /**
     * Constructs the instance
     *
     * @param classCount     count of classes
     * @param classReduction if true, classes will be merged having the same discretized value
     */
    public PercentileMedianDiscretizer(int classCount, boolean classReduction) {
        super(false);
        this.classCount = classCount;
        this.classReduction = classReduction;
    }

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels) {
        if (Stream.of(values).anyMatch(v -> !(v instanceof Number))) {
            throw new IllegalArgumentException("Only numeric values allowed for this discretizer");
        }

        List<DiscretizationTransition> result = new ArrayList<>();

        List<Number> numbers = Stream.of(values).map(i -> (Number) i)
                .sorted(Comparator.comparingDouble(Number::doubleValue))
                .collect(Collectors.toList());

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
            final Double medianValue = medianIndexValue(sublist);

            result.add(new DiscretizationTransition(
                    new NumericDiscretizationOrigin(sublist.get(0).doubleValue(),
                            sublist.get(sublist.size() - 1).doubleValue()), medianValue));

        }

        if (this.classReduction) {
            removeDuplicateDiscretizedValues(result);
            removeMaxIsMaxOfOther(result);
        }
        distinctMinAndMaxValues(numbers, result);

        return result;
    }

    /**
     * removes a transition if its max value equals the max value of the previous transition. This merges the two intervals
     * ]1-5) [5-5[  will be changed to ]1-5[
     * @param transitions transitions before remove
     */
    private void removeMaxIsMaxOfOther(List<DiscretizationTransition> transitions) {
        List<DiscretizationTransition> removableTransitions = new ArrayList<>(10);

        int index = 0;
        for (DiscretizationTransition transition : transitions) {
            for (int i = index + 1; i < transitions.size(); i++) {
                if (((NumericDiscretizationOrigin) transition.getDiscretizationOrigin()).getMaxValue()
                        .equals( ((NumericDiscretizationOrigin) transitions.get(i).getDiscretizationOrigin()).getMaxValue())){
                    removableTransitions.add(transitions.get(i));
                }
            }
            index++;
        }
        if (!removableTransitions.isEmpty()) {
            transitions.removeAll(removableTransitions);
        }
    }

    private void removeDuplicateDiscretizedValues(List<DiscretizationTransition> transitions) {

        List<Double> discretizedValues = transitions.stream()
                .map(DiscretizationTransition::getDiscretizedValue)

                .collect(Collectors.toList());
        if (discretizedValues.size() > new HashSet<>(discretizedValues).size()) {
            Map<Double, Long> valueCount = transitions.stream()
                    .map(DiscretizationTransition::getDiscretizedValue)
                    .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()));

            for (Map.Entry<Double, Long> entry : valueCount.entrySet()) {
                if (entry.getValue() > 1) {
                    List<DiscretizationTransition> discretizerRelationsWithSameCatValue = transitions.stream()
                            .filter(rel -> entry.getKey().equals(rel.getDiscretizedValue()))
                            .collect(Collectors.toList());
                    Optional<Double> conditionMinOptional = discretizerRelationsWithSameCatValue.stream()
                            .map(o -> ((NumericDiscretizationOrigin) o.getDiscretizationOrigin())
                                    .getMinValue().doubleValue()).min(Double::compareTo);
                    Optional<Double> conditionMaxOptional = discretizerRelationsWithSameCatValue.stream()
                            .map(o -> ((NumericDiscretizationOrigin) o.getDiscretizationOrigin())
                                    .getMaxValue().doubleValue()).max(Double::compareTo);

                    if (conditionMaxOptional.isPresent()) {
                        if (!classReduction) {
                            throw new IllegalArgumentException("Classcount too high, duplicate discretizedValues occur, " +
                                    "reduce classCount or allow Merging");
                        }
                        double conditionMin = conditionMinOptional.get();
                        double conditionMax = conditionMaxOptional.get();

                        Double discretizedValue = discretizerRelationsWithSameCatValue.get(0).getDiscretizedValue();

                        DiscretizationTransition combinedRelation = new DiscretizationTransition(
                                new NumericDiscretizationOrigin(conditionMin, conditionMax), discretizedValue);
                        transitions.removeAll(discretizerRelationsWithSameCatValue);
                        transitions.add(combinedRelation);
                        transitions.sort(Comparator.comparingDouble(DiscretizationTransition::getDiscretizedValue));
                    }
                }
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

    /**
     * as the apply function checks inclusively, neighboring transitions need to be distinct in max and min value. The
     * min value will be incremented to the next value.
     * @param numbers values to be discretized
     * @param transitions transitions to be evaluated
     */
    private static void distinctMinAndMaxValues(List<Number> numbers, List<DiscretizationTransition> transitions) {
        for (DiscretizationTransition transition : transitions) {
            Optional<DiscretizationTransition> relationWhereMinIsMaxOfOther = transitions.stream()
                    .filter(oTransition -> Objects.equals(
                            ((NumericDiscretizationOrigin) transition.getDiscretizationOrigin()).getMaxValue(),
                            ((NumericDiscretizationOrigin) oTransition.getDiscretizationOrigin()).getMinValue()))
                    .filter(oTransition -> transition != oTransition)
                    .findFirst();

            if (relationWhereMinIsMaxOfOther.isPresent()) {
                final DiscretizationTransition oTransition = relationWhereMinIsMaxOfOther.get();
                numbers.stream().map(Number::doubleValue)
                        .filter((number ->
                                number > ((NumericDiscretizationOrigin) oTransition.getDiscretizationOrigin()).getMinValue().doubleValue()))
                        .min(Double::compareTo)
                        .ifPresent(((NumericDiscretizationOrigin) oTransition.getDiscretizationOrigin())::setMinValue);
            }
        }
    }
}