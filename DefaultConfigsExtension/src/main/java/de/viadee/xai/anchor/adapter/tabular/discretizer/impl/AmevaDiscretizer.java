package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;

import java.io.Serializable;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * Implementation of the FUSINTER discretization algorithm described by [Gonzales-Abril, Cuberos, Velasco, Ortega 2009]
 */
public class AmevaDiscretizer extends AbstractDiscretizer {

    private Double[] targetValues;
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
        List<DiscretizationTransition> discretizationTransitions;
        List<Double> cutPoints;
        targetValues = Arrays.stream(labels).sorted().distinct().toArray(Double[]::new);

//        Input: Data consisting of N examples, l classes, and continuous variables X_i
//        For every X_i do:
//
//        Step 1: Initialization of the candidate interval
//        boundaries and the initial discretization scheme.

//        1.1 Find the maximum (d_k) and minimum (d_0) values of X_i.
//        1.2 Form a set of all distinct values of X_i, in ascending
//        order, and initialize all possible interval
//        boundaries B with the minimum, maximum and all the
//        midpoints of all the adjacent pairs in the set.

        final List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs = IntStream
                .range(0, values.length)
                .mapToObj(i -> new AbstractMap.SimpleImmutableEntry<>((Number) values[i], labels[i]))
                .sorted(Comparator.comparing(entry -> entry.getKey().doubleValue())).distinct()
                .collect(Collectors.toList());


        final List<Number> B;


        Number d_0 = keyValuePairs.get(0).getValue();
        Number d_k = keyValuePairs.get(keyValuePairs.size() - 1).getValue();

        double ameva, globalAmeva;
        globalAmeva = 0.0;

//        1.3 Set the initial discretization scheme to
//        L:{[d_0, d_k]}, set GlobalAmeva = 0.
//
//        Step 2: Consecutive additions of a new boundary which
//        results in the locally highest value of the Ameva
//        criterion.
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
//        while(ameva > globalAmeva){
//            double maxAmevaCrit = 0;
//            // add boundry not in list of boundry from list of candidate and calculate Ameva
//            k++;
//            if( k) {
//            }
//
//            cutPoints.add(point);
//
//            globalAmeva = ameva;
//        }

//        Output: Discretization scheme L(k).
        return null;
    }
}
