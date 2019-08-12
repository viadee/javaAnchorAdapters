package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Interval;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class MDLPDiscretizer extends AbstractDiscretizer {

    private List<AbstractMap.SimpleImmutableEntry<Double, Double>> keyValuePairs;
    private List<Integer> potentialCutPoints = new ArrayList<>();
    private List<Integer> actualIntervalEnds = new ArrayList<>();
    private Double[] targetValues;

    public MDLPDiscretizer() {
        super(true);
    }

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels) {

       keyValuePairs = IntStream.range(0, values.length)
                .mapToObj(i -> new AbstractMap.SimpleImmutableEntry<>((Double) values[i], labels[i]))
                .sorted(Comparator.comparing(AbstractMap.SimpleImmutableEntry::getKey))
                .collect(Collectors.toList());

        targetValues = keyValuePairs.stream().map(AbstractMap.SimpleImmutableEntry::getValue).sorted().distinct().toArray(Double[]::new);

        List<Interval> initialSplit = equalClassSplit(keyValuePairs);


        for(Interval interval: initialSplit) {
            potentialCutPoints.add(interval.getEnd());
        }
        determineIntervals(0, keyValuePairs.size() - 1);

        List<Interval> evaluatedIntervals = new ArrayList<>();
        Collections.sort(actualIntervalEnds);
        int begin = 0;
        for(Integer end: actualIntervalEnds) {
            evaluatedIntervals.add(new Interval(begin, end, keyValuePairs));
            begin = end + 1;
        }
        evaluatedIntervals.add(new Interval(begin, keyValuePairs.size() - 1, keyValuePairs));

        return evaluatedIntervals.stream().map(Interval::toDiscretizationTransition).collect(Collectors.toList());
    }

    /**
     * generates initial Intervals. Values with same class are merged to a Interval. If a value has several classes, all index with
     * this value will be a separate Interval.
     *
     * @param keyValuePairs, Array of Attribute Class.
     * @return initial List of Intervals
     */
    private List<Interval> equalClassSplit(final List<AbstractMap.SimpleImmutableEntry<Double, Double>> keyValuePairs) {
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
                    if ( !resultDiscTrans.isEmpty() && resultDiscTrans.get(resultDiscTrans.size() - 1).getEnd() != i - amountSameValue - 1) {
                        resultDiscTrans.add(new Interval(lowerLimit, i - 1 - amountSameValue, keyValuePairs));
                    }
                    lowerLimit = i - amountSameValue;
                    i++;
                    if(i == keyValuePairs.size()){
                        break;
                    }
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

    private void determineIntervals(int begin, int end) {
        double mdlpcMax = 0;
        int valueMax = -1;
        int posMax = -1;
        List<Integer> reducedPotentialIntervalEnds = potentialCutPoints.stream()
                .filter(intervalEnd -> intervalEnd >= begin
                                    && intervalEnd <= end)
                .collect(Collectors.toList());
        for(int i = 0; i < reducedPotentialIntervalEnds.size(); i++) {
            double mdlpc = determineMDLPCCriterion(begin, end, reducedPotentialIntervalEnds.get(i));
            if(mdlpcMax < mdlpc) {
                mdlpcMax = mdlpc;
                posMax = i;
                valueMax = reducedPotentialIntervalEnds.get(i);
            }
        }

        if(mdlpcMax > 0) {
            actualIntervalEnds.add(valueMax);
            determineIntervals(begin, reducedPotentialIntervalEnds.get(posMax));
            determineIntervals(reducedPotentialIntervalEnds.get(posMax) + 1, end);
        }
    }

    private double determineMDLPCCriterion(Integer begin, Integer end, Integer i) {
        Interval completeInterval = new Interval(begin, end, keyValuePairs);
        Interval leftInterval = new Interval(begin, i, keyValuePairs);
        long leftCD = Arrays.stream(leftInterval.getClassDist()).filter(label -> label != 0).count();
        Interval rightInterval = new Interval(i + 1, end, keyValuePairs);
        long rightCD = Arrays.stream(rightInterval.getClassDist()).filter(label -> label != 0).count();

        double entropyComplete = computeEntropy(completeInterval);
        double entropyLeft = computeEntropy(leftInterval);
        double entropyRight = computeEntropy(rightInterval);

        double gain = entropyComplete - ((leftInterval.getSize() / (double) completeInterval.getSize()) * entropyLeft + (rightInterval.getSize() / (double) completeInterval.getSize()) * entropyRight);
//        double gain = entropyComplete - (entropyLeft + entropyRight);
        double delta = log2(Math.pow(3, targetValues.length) - 2)
                        - (targetValues.length * entropyComplete
                        - leftCD * entropyLeft
                        - rightCD * entropyRight);
        return gain - (log2(completeInterval.getSize() - 1.0)) / (double) completeInterval.getSize()
                    - delta / (double) completeInterval.getSize();
    }

    private double computeEntropy(Interval interval) {
        double entropy = 0;
        for(int i = 0; i < targetValues.length; i++) {
            entropy += (interval.getClassDist()[i] / (double) interval.getSize())
                    * log2((interval.getClassDist()[i] / (double) interval.getSize()));
        }
        return -1 * entropy;
    }

    private double log2(double value) {
        if(value == 0D) {
            return 0D;
        }
        return Math.log(value)/Math.log(2);
    }
}
