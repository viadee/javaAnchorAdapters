package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.IntStream;

/**
 * Implementation of the FUSINTER discretization Algorithm described by [Zighed, Rabaséda, Rakotomalala 1998]
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

    List<Interval> fitCreateSupervisedTransitions(Number[][] values) {

        targetValues = IntStream.range(0, values.length)
                .map(i -> values[i][1].intValue()).sorted().distinct().toArray();
        m = targetValues.length;
        n = values.length;

        // 1.  Sort the values
        // TODO: make the sorting parallel(Streaming?)
        Arrays.sort(values, new Comparator<Number[]>() {

            @Override
            // Compare values according to value
            public int compare(Number[] array1,
                               Number[] array2) {

                Double value1 = array1[0].doubleValue();
                Double value2 = array2[0].doubleValue();
                return value1.compareTo(value2);
            }
        });
        // 2. Generate initial Intervals
        List<Interval> equalClassSplits;
        equalClassSplits = equalClassSplit(values);

        // 3.  Evaluate if merge of two neighboring discRelations improves criterion.
        List<Interval> evaluatedIntervals;
        evaluatedIntervals = evaluateIntervals(equalClassSplits, values);

        // 4. Return list of Intervals/DiscretizationTransitions/Cut Points
        // List<Interval> --> List<DiscretizationTransition>
        return evaluatedIntervals;
    }

    // TODO: Serializable[][] parameter
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
                //TODO: Fix for value 17 - 18 - 19 in FUSINTER example.
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

        // will be set FALSE if no improvement is possible in this iteration (1. exit-condition)
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
                // merge Intervals of at index i
                equalClassSplits = mergeInterval(equalClassSplits, deleteIndex, values);
            } else {
                improvement = false;
            }
        }

        return equalClassSplits;
    }

    //TODO: write more tests
    private double determineDiscCrit(List<Interval> intervals) {
        double criterion = 0;
        double intervalSum = 0;
        for (int j = 0; j < intervals.size(); j++) {
            double intervalXclassSum = 0;
            for (int i = 0; i < m; i++) {
                double quotient = (intervals.get(j).getClassDist()[i] + lambda) / (intervals.get(j).getSize() + m * lambda);
                intervalXclassSum += (quotient * (1 - quotient));
            }
            intervalSum = alpha * ((intervals.get(j).getSize()) / (double) n) * intervalXclassSum + ((1 - alpha) * ((m * lambda) / (intervals.get(j).getSize())));

            criterion += intervalSum;
        }


        return criterion;
    }

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
         * private Interval class for FUSINTER Method with begin and end as int for indexing over (2D-Array)
         */

        private int begin;
        private int end;
        private int size;
        private int[] classDist = new int[targetValues.length];

        /**
         * @param begin begin index of Interval
         * @param end   end index of Interval
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