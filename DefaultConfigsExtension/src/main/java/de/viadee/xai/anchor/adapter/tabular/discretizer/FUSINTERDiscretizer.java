package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.IntStream;

/**
 * Implementation of the FUSINTER discretization algorithm described by [Zighed, Rabaséda, Rakotomalala 1998]
 */
public class FUSINTERDiscretizer extends AbstractDiscretizer {

    private final double lambda;
    private final double alpha;

    // All possible classification in values
    private int[] targetValues;
    // Number of possible classifications
    private int m;
    // Number of instances
    private int n;

    /**
     * generates a FUSINTER discretizer with parameters suggested by the authors
     */
    public FUSINTERDiscretizer() {
        this(1.0, 0.975);
    }

    public FUSINTERDiscretizer(double lambda, double alpha) {
        this.lambda = lambda;
        this.alpha = alpha;
    }

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values) {
        return null;
    }

    /**
     * TODO: Serializable[][] parameter
     * Implementation of FUSINTER, 1. sort, 2. equalClassIntervals 3. merge if entropy improves
     * @param values array of arrays of type {Number, int}, 1. is the column to be discretized
     *               and 2. is the classification
     * @return list of Intervals determined to have the highest entropy
     */
    List<Interval> fitCreateSupervisedTransitions(Number[][] values) {

        targetValues = Arrays.stream(values).mapToInt(value -> value[1].intValue()).sorted().distinct().toArray();
        m = targetValues.length;
        n = values.length;

        Arrays.sort(values, Comparator.comparing(value -> value[0].doubleValue()));
        List<Interval> equalClassSplits = equalClassSplit(values);
        List<Interval> evaluatedIntervals;
        evaluatedIntervals = evaluateIntervals(equalClassSplits, values);

        // return list of Intervals/DiscretizationTransitions/Cut Points
        // List<Interval> --> List<DiscretizationTransition>
        return evaluatedIntervals;
    }

    /**
     * generates initial Intervals. Values with same class together, if a value has several classes, all index with
     * this value will be a separate Interval.
     * @param values, Array of Attribute Class.
     * @return initial List of Intervals
     */
    List<Interval> equalClassSplit(Number[][] values) {
        final List<Interval> resultDiscTrans = new ArrayList<>();
        int lowerLimit = 0;
        int amountSameValue = 0;
        for (int i = 1; i < values.length; i++) {
            if (!values[i][0].equals(values[i - 1][0])) {
                amountSameValue = 0;
                if (!values[i][1].equals(values[i - 1][1])) {
                    resultDiscTrans.add(new Interval(lowerLimit, i - 1, values));
                    lowerLimit = i;
                }
            } else {
                amountSameValue++;
                if (!values[i][1].equals(values[i - amountSameValue][1])) {
                    if(resultDiscTrans.get(resultDiscTrans.size() -1).getEnd() != i - amountSameValue -1) {
                        resultDiscTrans.add(new Interval(lowerLimit, i - 1 - amountSameValue, values));
                    }
                    lowerLimit = i - amountSameValue;
                    i++;
                    while (values[i][0].equals(values[i - 1][0])) {
                        i++;
                    }
                    resultDiscTrans.add(new Interval(lowerLimit, i - 1, values));
                    lowerLimit = i;
                    amountSameValue = 0;
                }
            }
        }
        resultDiscTrans.add(new Interval(lowerLimit, values.length - 1, values));

        return resultDiscTrans;
    }

    private List<Interval> evaluateIntervals(List<Interval> equalClassSplits, Number[][] values) {
        boolean improvement = true;

        while (equalClassSplits.size() > 1 && improvement) {
            int deleteIndex = 0;
            double maxMergeCrit = 0.0;
            double oldCrit = determineDiscCrit(equalClassSplits);
            for (int i = 0; i < equalClassSplits.size() - 1; i++) {
                List<Interval> possibleMergedInterval = mergeInterval(equalClassSplits, i, values);
                final double mergeCrit = oldCrit - determineDiscCrit(possibleMergedInterval);
                if (mergeCrit > maxMergeCrit) {
                    deleteIndex = i;
                    maxMergeCrit = mergeCrit;
                }
            }

            if (maxMergeCrit > 0) {
                equalClassSplits = mergeInterval(equalClassSplits, deleteIndex, values);
            } else {
                improvement = false;
            }
        }

        return equalClassSplits;
    }

    private double determineDiscCrit(List<Interval> intervals) {
        double criterion = 0;
        for(Interval interval: intervals) {
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
     * Interval i begin = 0, i end = 5
     * Interval i+1 begin = 6, i+1 end = 9
     * -> Interval j begin = 0, end 9
     * @param intervals list of Interval to be reduced by one Element
     * @param i index of element that will be merged
     * @param values list of values used for index
     * @return new List with Interval i and i+1 merged
     */
    private List<Interval> mergeInterval(List<Interval> intervals, int i, Number[][] values) {
        List<Interval> temp = new ArrayList<>(intervals.subList(0, intervals.size()));
        int mergeBegin = temp.get(i).getBegin();
        int mergeEnd = temp.get(i + 1).getEnd();
        temp.add(i, new Interval(mergeBegin, mergeEnd, values));
        temp.remove(i + 1);
        temp.remove(i + 1);

        return temp;
    }

    final class Interval {
        /**
         * private Interval class for FUSINTER Method with begin and end index to determine class distribution
         */

        private int begin;
        private int end;
        private int size;
        private int[] classDist = new int[targetValues.length];

        /**
         * @param begin begin index of Interval
         * @param end   end index of Interval
         * @param values list of all values, only used to determine class distribution in interval
         */
        Interval(int begin, int end, Number[][] values) {
            this.begin = begin;
            this.end = end;
            this.size = end - begin + 1;

            for (int t = 0; t < targetValues.length; t++) {
                int finalT = t;
                int[] finalTargetValues = targetValues;
                classDist[t] = (int) IntStream.rangeClosed(begin, end)
                        .map(i -> values[i][1].intValue()).filter(i -> i == finalTargetValues[finalT])
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

        public int getSize() {
            return size;
        }
    }
}