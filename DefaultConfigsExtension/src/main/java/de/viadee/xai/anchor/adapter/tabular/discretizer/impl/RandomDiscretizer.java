package de.viadee.xai.anchor.adapter.tabular.discretizer.impl;

import de.viadee.xai.anchor.adapter.tabular.discretizer.AbstractDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationTransition;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RandomDiscretizer extends AbstractDiscretizer {

    public RandomDiscretizer() {
        super(false);
    }

    @Override
    protected List<DiscretizationTransition> fitCreateTransitions(Serializable[] values, Double[] labels) {
        if (Stream.of(values).anyMatch(v -> !(v instanceof Number))) {
            throw new IllegalArgumentException("Only numeric values allowed for this discretizer");
        }

        final List<Double> valsDistinct = Stream.of(values).distinct().map(v -> ((Number) v).doubleValue()).sorted().collect(Collectors.toList());

        Random random = new Random();

        int numberCuts = random.nextInt(valsDistinct.size() + 1);
        Double[] cutPoints = new Double[numberCuts];

        Collections.shuffle(valsDistinct);
        for (int i = 0; i < numberCuts; i++) {
            cutPoints[i] = valsDistinct.get(i);
        }

        ManualDiscretizer manualDiscretizer = new ManualDiscretizer(cutPoints);

        return manualDiscretizer.fitCreateTransitions(values, null);
    }
}
