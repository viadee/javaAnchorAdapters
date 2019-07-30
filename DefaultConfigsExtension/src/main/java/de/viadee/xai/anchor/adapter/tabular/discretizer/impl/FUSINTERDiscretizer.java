package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;
import de.viadee.xai.anchor.adapter.tabular.discretizer.NumericDiscretizationOrigin;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Implementation of the FUSINTER discretization algorithm described by [Zighed, Rabaséda, Rakotomalala 1998]
 */
public class FUSINTERDiscretizer extends AbstractDiscretizer {

    private final double lambda;
    private final double alpha;

    // All possible classification in values
    private Double[] targetValues;
    // Number of possible classifications
    private int m;
    // Number of instances
    private int n;

    /**
     * Generates a FUSINTER discretizer with parameters suggested by the authors
     */
    public FUSINTERDiscretizer() {
        this(1.0, 0.975);
    }

    /**
     * Generates a FUSINTER discretizer with custom parameters
     *
     * @param lambda lambda value
     * @param alpha  alpha value
     */
    public FUSINTERDiscretizer(double lambda, double alpha) {
        super(true);
        this.lambda = lambda;
        this.alpha = alpha;
    }

    /*
     *
     * Implementation of FUSINTER, 1. sort, 2. equalClassIntervals 3. merge if entropy improves
     *
     * @param values array of arrays of type {Number, int}, 1. is the column to be discretized
     *               and 2. is the classification
     * @return list of Intervals determined to have the highest entropy
     */
    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels) {

        if (Stream.of(values).anyMatch(v -> !(v instanceof Number))) {
            throw new IllegalArgumentException("Only numeric values allowed for this discretizer");
        }

        targetValues = Arrays.stream(labels).sorted().distinct().toArray(Double[]::new);

        m = targetValues.length;
        n = values.length;

        final List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs = IntStream
                .range(0, values.length)
                .mapToObj(i -> new AbstractMap.SimpleImmutableEntry<>((Number) values[i], labels[i]))
                .sorted(Comparator.comparing(entry -> entry.getKey().doubleValue()))
                .collect(Collectors.toList());

        List<Interval> equalClassSplits = equalClassSplit(keyValuePairs);
        List<Interval> evaluatedIntervals;
        evaluatedIntervals = evaluateIntervals(equalClassSplits, keyValuePairs);

        return evaluatedIntervals.stream().map(Interval::toDiscretizationTransition).collect(Collectors.toList());
    }

    /**
     * generates initial Intervals. Values with same class are merged to a Interval. If a value has several classes, all index with
     * this value will be a separate Interval.
     *
     * @param keyValuePairs, Array of Attribute Class.
     * @return initial List of Intervals
     */
    List<Interval> equalClassSplit(final List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs) {
        final List<Interval> resultDiscTrans = new ArrayList<>();
        int lowerLimit = 0;
        int amountSameValue = 0;
        for (int i = 1; i < keyValuePairs.size(); i++) {
            final Number currentKey = keyValuePairs.get(i).getKey();
            final Double currentValue = keyValuePairs.get(i).getValue();

            if (!currentKey.equals(keyValuePairs.get(i - 1).getKey())) {
                amountSameValue = 0;
                if (!currentValue.equals(keyValuePairs.get(i - 1).getValue())) {
                    resultDiscTrans.add(new Interval(lowerLimit, i - 1, keyValuePairs));
                    lowerLimit = i;
                }
            } else {
                amountSameValue++;
                if (!currentValue.equals(keyValuePairs.get(i - amountSameValue).getValue())) {
                    if (resultDiscTrans.get(resultDiscTrans.size() - 1).getEnd() != i - amountSameValue - 1) {
                        resultDiscTrans.add(new Interval(lowerLimit, i - 1 - amountSameValue, keyValuePairs));
                    }
                    lowerLimit = i - amountSameValue;
                    i++;

                    while (keyValuePairs.get(i).getKey().equals(keyValuePairs.get(i - 1).getKey())) {
                        i++;
                    }

                    resultDiscTrans.add(new Interval(lowerLimit, i - 1, keyValuePairs));
                    lowerLimit = i;
                    amountSameValue = 0;
                }
            }
        }
        resultDiscTrans.add(new Interval(lowerLimit, keyValuePairs.size() - 1, keyValuePairs));

        return resultDiscTrans;
    }

    private List<Interval> evaluateIntervals(List<Interval> equalClassSplits,
                                             final List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs) {
        boolean improvement = true;

        while (equalClassSplits.size() > 1 && improvement) {
            int deleteIndex = 0;
            double maxMergeCrit = 0.0;
            double oldCrit = determineDiscretizationCriterion(equalClassSplits);
            for (int i = 0; i < equalClassSplits.size() - 1; i++) {
                List<Interval> possibleMergedInterval = mergeInterval(equalClassSplits, i, keyValuePairs);
                final double mergeCrit = oldCrit - determineDiscretizationCriterion(possibleMergedInterval);
                if (mergeCrit > maxMergeCrit) {
                    deleteIndex = i;
                    maxMergeCrit = mergeCrit;
                }
            }

            if (maxMergeCrit > 0) {
                equalClassSplits = mergeInterval(equalClassSplits, deleteIndex, keyValuePairs);
            } else {
                improvement = false;
            }
        }

        return equalClassSplits;
    }

    /**
     * determines the Entropy of the given Intervals with the quadratic entropy formula
     *
     * @param intervals to be evaluated
     * @return double from 0 to 1 with 0 being perfect entropy
     */
    private double determineDiscretizationCriterion(List<Interval> intervals) {
        double criterion = 0;
        for (Interval interval : intervals) {
            double intervalClassSum = 0;
            for (int i = 0; i < m; i++) {
                double quotient = (interval.getClassDist()[i] + lambda) / (interval.getSize() + m * lambda);
                intervalClassSum += (quotient * (1 - quotient));
            }
            double intervalSum = alpha * ((interval.getSize()) / (double) n) * intervalClassSum;
            intervalSum += ((1 - alpha) * ((m * lambda) / (interval.getSize())));

            criterion += intervalSum;
        }

        return criterion;
    }

    /**
     * merges Interval i and i+1,
     *
     * @param intervals     list of Interval to be reduced by one Element
     * @param i             index of element that will be merged
     * @param keyValuePairs list of values used for index
     * @return new List with Interval i and i+1 merged
     */
    private List<Interval> mergeInterval(List<Interval> intervals, int i,
                                         final List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs) {
        List<Interval> temp = new ArrayList<>(intervals.subList(0, intervals.size()));
        int mergeBegin = temp.get(i).getBegin();
        int mergeEnd = temp.get(i + 1).getEnd();
        temp.add(i, new Interval(mergeBegin, mergeEnd, keyValuePairs));
        temp.remove(i + 1);
        temp.remove(i + 1);

        return temp;
    }

    final class Interval {
        /**
         * private Interval class for FUSINTER Method with begin and end index to determine class distribution
         */

        private final int begin;
        private final int end;
        private final int size;
        private final int[] classDist = new int[targetValues.length];
        private final List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs;

        /**
         * @param begin         begin index of Interval
         * @param end           end index of Interval
         * @param keyValuePairs list of all values, only used to determine class distribution in interval
         */
        Interval(int begin, int end, List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs) {
            this.begin = begin;
            this.end = end;
            this.size = end - begin + 1;
            this.keyValuePairs = keyValuePairs;

            for (int t = 0; t < targetValues.length; t++) {
                final int finalT = t;
                Double[] finalTargetValues = targetValues;
                classDist[t] = (int) IntStream.rangeClosed(begin, end)
                        .mapToObj(i -> keyValuePairs.get(i).getValue())
                        .filter(i -> Double.compare(i, finalTargetValues[finalT]) == 0)
                        .count();
            }
        }

        int getBegin() {
            return begin;
        }

        int getEnd() {
            return end;
        }

        int[] getClassDist() {
            return classDist;
        }

        int getSize() {
            return size;
        }

        /**
         * @return the interval as a {@link DiscretizationTransition}
         */
        private DiscretizationTransition toDiscretizationTransition() {
            return new DiscretizationTransition(new NumericDiscretizationOrigin(
                    keyValuePairs.get(begin).getKey(),
                    keyValuePairs.get(end).getKey()),
                    medianIndexValue()
            );
        }

        double medianIndexValue() {
            if (getSize() % 2 == 0) {
                return (keyValuePairs.get(end + 1 - getSize() / 2).getKey().doubleValue() + keyValuePairs.get(end + 1 - getSize() / 2 - 1).getKey().doubleValue()) / 2;
            } else {
                return keyValuePairs.get(end + 1 - getSize() / 2).getKey().doubleValue();
            }
        }
    }
}