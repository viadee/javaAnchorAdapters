package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Interval;
import de.viadee.xai.anchor.adapter.tabular.discretizer.NumericDiscretizationOrigin;

import java.io.Serializable;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Implementation of the FUSINTER discretization algorithm described by [Gonzales-Abril, Cuberos, Velasco, Ortega 2009]
 */
public class AmevaDiscretizer extends AbstractDiscretizer {

    private Double[] targetValues;
    private long[] targetValueDistribution;
    private List<Double> B = new ArrayList<>();
    private List<Double> actualCutPoints = new ArrayList<>();
    private List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs;

    /**
     * Constructs the Ameva Discretizer, Ameva works without any Parameters.
     */
    public AmevaDiscretizer() {
        super(true);
    }

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels) {

        if (Stream.of(values).anyMatch(v -> !(v instanceof Number))) {
            throw new IllegalArgumentException("Only numeric values allowed for this discretizer");
        }
        List<DiscretizationTransition> discretizationTransitions = new ArrayList<>();;
        targetValues = Arrays.stream(labels).sorted().distinct().toArray(Double[]::new);
        targetValueDistribution = new long[targetValues.length];
        for(int i = 0; i < targetValues.length; i++) {
            int finalI = i;
            targetValueDistribution[i] = Arrays.stream(labels).filter(label -> label.equals(targetValues[finalI])).count();
        }
//        For every continuous variable (X_i) do:
//
//        Step 1: Initialization of the candidate interval
//        boundaries and the initial discretization scheme.

//        1.1 Find the maximum (d_k) and minimum (d_0) values of X_i.
//        1.2 Form a set of all distinct values of X_i, in ascending
//        order, and initialize all possible interval
//        boundaries B with the minimum, maximum and all the
//        midpoints of all the adjacent pairs in the set.

        keyValuePairs = IntStream.range(0, values.length)
                .mapToObj(i -> new AbstractMap.SimpleImmutableEntry<>((Number) values[i], labels[i]))
                .sorted(Comparator.comparing(entry -> entry.getKey().doubleValue()))
                .collect(Collectors.toList());

        B = IntStream.range(1, values.length)
                .mapToObj(i -> ((Double) values[i] + (Double) values[i-1] )/ 2)
                .sorted().distinct()
                .collect(Collectors.toList());

//        1.3 Set the initial discretization scheme to
//        L:{[d_0, d_k]}, set GlobalAmeva = 0.
        Number d_0 = keyValuePairs.get(0).getValue();
        Number d_k = keyValuePairs.get(keyValuePairs.size() - 1).getValue();

        discretizationTransitions.add(new DiscretizationTransition(new NumericDiscretizationOrigin(
                d_0,
                d_k),
                (Double) values[values.length / 2]
        ));
        double globalAmeva = 0.0;


//        Step 2: Consecutive additions of a new boundary which
//        results in the locally highest value of the Ameva
//        criterion.
//
//
//        2.1 Initialize k = 1;
        int k = 1;

//        2.2 Tentatively add an inner boundary, which is not
//        already in L, from B, and calculate the corresponding
//        Ameva value.
//        2.3 After all the tentative additions have been tried,
//        accept the one with the highest value of Ameva.
//        2.4 If (Ameva > GlobalAmeva ) then update L with the
//        accepted boundary in step 2.3 and set
//        GlobalAmeva = Ameva , else terminate.
//        2.5 Set k = k + 1 and go to 2.2

        double ameva = createNewCutPoint(0.0);

        while(ameva > globalAmeva){
            globalAmeva = ameva;
            // add new boundary and calculate Ameva
            ameva = createNewCutPoint(globalAmeva);
        }

        Collections.sort(actualCutPoints);

        List<Interval> evaluatedIntervals = getIntervalsFromCutPoints(actualCutPoints);
//        Output: Discretization scheme L(k).
        return evaluatedIntervals.stream().map(Interval::toDiscretizationTransition).collect(Collectors.toList());
    }

    private double createNewCutPoint(double currentAmeva) {
        double maxAmeva = currentAmeva;
        int maxPos = 0;
        for(int i = 0; i < B.size(); i++) {
            double potentialCutPoint = B.get(0);
            double ameva = determineAmeva(potentialCutPoint);

            if(ameva > maxAmeva) {
                maxPos = i;
                maxAmeva = ameva;
            }
        }
        if(maxAmeva != currentAmeva) {
            actualCutPoints.add(B.get(maxPos));
            B.remove(maxPos);
        }

        return maxAmeva;
    }

    private double determineAmeva(double potentialCutPoint) {
        List<Double> potentialCutPoints = new ArrayList<>(actualCutPoints);
        potentialCutPoints.add(potentialCutPoint);
        Collections.sort(potentialCutPoints);
        List<Interval> intervals = getIntervalsFromCutPoints(potentialCutPoints);

        double chiSquared = 0.0;
        for(int i = 0; i < targetValues.length; i++){
            for(int j = 0; j < intervals.size(); j++){
                double dividend = Math.pow(intervals.get(j).getClassDist()[i], 2);
                double divisor = targetValueDistribution[i] * intervals.get(j).getSize();
                chiSquared += dividend / divisor;
            }
        }
        chiSquared = (keyValuePairs.size()) *  (-1 + chiSquared) /* N * ( -1 + chi²) */;

        double ameva = chiSquared / (intervals.size() * (targetValues.length  - 1)) /*k * (l - 1)*/;
        return ameva;
    }

    private List<Interval> getIntervalsFromCutPoints(List<Double> cutPoints) {
        int lowerBoundary = 0;
        int z = 0;
        List<Interval> createdIntervals = new ArrayList<>();
        for(Double cp: cutPoints) {
            while(keyValuePairs.get(z).getKey().doubleValue() < cp && z < keyValuePairs.size()) {
                z++;
            }
            createdIntervals.add(new Interval(lowerBoundary, z, keyValuePairs));
            z = lowerBoundary = z + 1;
        }
        if(createdIntervals.get(createdIntervals.size() -1).getEnd() != keyValuePairs.get(keyValuePairs.size() -1).getKey().doubleValue()) {
            createdIntervals.add(new Interval(lowerBoundary, keyValuePairs.size() - 1, keyValuePairs));
        }
        return createdIntervals;
    }
}
