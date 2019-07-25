package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * Implementation of the FUSINTER discretization Algorithm described by [Zighed, Rabaséda, Rakotomalala 1998]
 */
public class FUSINTERDiscretizer extends AbstractDiscretizer {

    private final double lambda;
    private final double alpha;

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
    final class Interval {
        /**
         * private Interval class for FUSINTER Method with begin and end as int for indexing over (2D-Array)
         */

        private int begin;
        private int end;
//        private final Number []cd;

        /**
         *
         * @param begin beginindex of Interval
         * @param end endindex of Interval
         * TODO: add values to parameters (?) and get Numberstream von classdistribution
         */
        Interval(int begin,int end) {
            this.begin= begin;
            this.end= end;
//            computeIntervalRatios();
        }

//        void computeIntervalRatios() {
//            cd=classDistribution(attribute,values,begin,end);
//        }
//
//        /**
//         * <p>
//         * Enlarge the interval using a new "end"
//         * </p>
//         * @param newEnd indicates the new end
//         */
//        public void enlargeInterval(int newEnd) {
//            end=newEnd;
//            computeIntervalRatios();
//        }
    }
    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values) {
        return null;
    }

//    protected List<DiscretizationTransition> fitCreateSupervisedTransitionsSer(Serializable[][] values) {

        // 1. Sort the values
//        List<Number> sortedList = Stream.of(values).map(i -> (Number) i)
//                .sorted(Comparator.comparingDouble(Number::doubleValue))
//                .collect(Collectors.toList());

        // 2. Generate initial Intervals

        // 3. Evaluate if merge of two neighboring discRelations improves criterion.

        // 4. Return list of DiscretizationTransitions
//        return null;
//    }

    List<Interval> fitCreateSupervisedTransitions(Number[][] values) {
//        if (Stream.of(values).anyMatch(v -> !(v instanceof Number[]))) {
//            throw new IllegalArgumentException("Only numeric values allowed for this discretizer");
//        }

        // 1.  Sort the values
        // TODO: parallise the sorting
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

        // 4. Return list of DiscretizationTransitions

//        List<Interval> --> List<DiscretizationTransition>
        return equalClassSplits;
    }

    List<Interval> equalClassSplit(Number[][] values) {
        final List<Interval> resultDiscTrans = new ArrayList<>();
        int lowerLimit = 0;
        int amountSameValue = 0;
        for(int i = 1; i < values.length; i++) {
            if(!values[i][0].equals(values[i-1][0])){
                amountSameValue = 0;
                if(!values[i][1].equals(values[i-1][1])){
                    resultDiscTrans.add(new Interval(lowerLimit, i-1));
                    lowerLimit = i;
                }
            } else {
                amountSameValue++;
                if(!values[i][1].equals(values[i-amountSameValue][1])){
                    resultDiscTrans.add(new Interval(lowerLimit, i - 1 - amountSameValue));
                    lowerLimit = i - amountSameValue;
                    i++;
                    while(values[i][0].equals(values[i-1][0])){
                        i++;
                    }
                    resultDiscTrans.add(new Interval(lowerLimit, i-1));
                    lowerLimit = i;
                }
            }
        }
        resultDiscTrans.add(new Interval(lowerLimit, values.length -1));


        return resultDiscTrans;
    }

    private List<Interval> evaluateIntervals(List<Interval> equalClassSplits, Number[][] values) {
        double alpha; // = this.alpha;
        double lambda; // = this.lambda;
        int m; // Number of targetValues
        int n = values.length - 1; // Number of instances
        int n_j; // end - begin Number of instances in interval (j)
        int nij; // Number of instances with classification (i) in interval (j)


        return null;
    }
}