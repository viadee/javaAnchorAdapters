package de.viadee.anchorj.tabular.discretizer;

import org.apache.commons.math3.stat.descriptive.rank.Percentile;

import java.util.LinkedHashSet;
import java.util.stream.Stream;

/**
 * Discretizes data by using percentiles
 */
public class NominalPercentileDiscretizer extends AbstractNumericDiscretizer {

    private final int[] percentiles;
    private LinkedHashSet<Double> fittedPercentiles;

    /**
     * Instantiates a new Percentile discretizer.
     *
     * @param percentiles the percentiles
     */
    public NominalPercentileDiscretizer(int[] percentiles) {
        this.percentiles = percentiles;
    }

    // Binary search would be more efficient, however, array is expected to be small (<10 entries)
    private static int searchSorted(LinkedHashSet<Double> set, Number x) {
        int i = 0;
        for (Double current : set) {
            if (x.doubleValue() <= current)
                break;
            i++;
        }
        return i;
    }

    @Override
    public void fit(Object[] values) {
        double[] columnAsDouble = Stream.of(values).mapToDouble(v -> ((Number) v).doubleValue()).toArray();
        fittedPercentiles = new LinkedHashSet<>();
        Percentile percentile = new Percentile();
        for (int p : percentiles) {
            double evaluate = percentile.evaluate(columnAsDouble, p);
            fittedPercentiles.add(evaluate);
        }
    }

    @Override
    public boolean isResultNumeric() {
        return true;
    }

    @Override
    public Integer apply(Object o) {
        return searchSorted(fittedPercentiles, (Number) o);
    }
}
