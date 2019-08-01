package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.util.AbstractMap;
import java.util.List;
import java.util.stream.IntStream;

public final class Interval {
    /**
     * private Interval class for FUSINTER Method with begin and end index to determine class distribution
     */

    private final int begin;
    private final int end;
    private final int size;
    private final int[] classDist;
    private final List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs;

    /**
     * @param begin         begin index of Interval
     * @param end           end index of Interval
     * @param keyValuePairs list of all values, only used to determine class distribution in interval
     */
    public Interval(int begin, int end, List<AbstractMap.SimpleImmutableEntry<Number, Double>> keyValuePairs) {
        Double[] targetValues = keyValuePairs.stream().map(AbstractMap.SimpleImmutableEntry::getValue).sorted().distinct().toArray(Double[]::new);
        this.classDist = new int[targetValues.length];
        this.begin = begin;
        this.end = end;
        this.size = end - begin + 1;
        this.keyValuePairs = keyValuePairs;

        for (int t = 0; t < targetValues.length; t++) {
            final int finalT = t;
            classDist[t] = (int) IntStream.rangeClosed(begin, end)
                    .mapToObj(i -> keyValuePairs.get(i).getValue())
                    .filter(i -> Double.compare(i, targetValues[finalT]) == 0)
                    .count();
        }
    }

    public int getBegin() {
        return begin;
    }

    public int getEnd() {
        return end;
    }

    public int[] getClassDist() {
        return classDist;
    }

    public int getSize() {
        return size;
    }

    /**
     * @return the interval as a {@link DiscretizationTransition}
     */
    public DiscretizationTransition toDiscretizationTransition() {
        return new DiscretizationTransition(new NumericDiscretizationOrigin(
                keyValuePairs.get(begin).getKey(),
                keyValuePairs.get(end).getKey()),
                medianIndexValue()
        );
    }

    private double medianIndexValue() {
        if (getSize() % 2 == 0) {
            return (keyValuePairs.get(end + 1 - getSize() / 2).getKey().doubleValue() + keyValuePairs.get(end + 1 - getSize() / 2 - 1).getKey().doubleValue()) / 2;
        } else {
            return keyValuePairs.get(end + 1 - getSize() / 2).getKey().doubleValue();
        }
    }
}