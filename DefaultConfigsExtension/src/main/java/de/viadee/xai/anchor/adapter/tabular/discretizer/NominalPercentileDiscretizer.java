package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;
import java.util.LinkedHashSet;
import java.util.stream.Stream;

import org.apache.commons.math3.stat.descriptive.rank.Percentile;

/**
 * Discretizes data by using percentiles.
 * <p>
 * Creates new groups ranging from 0..percentileCount
 */
public class NominalPercentileDiscretizer implements Discretizer {
    private static final long serialVersionUID = 6338528106659032376L;

    private final int[] percentiles;
    private LinkedHashSet<Double> fittedPercentiles;

    /**
     * Instantiates a new Percentile discretizer.
     *
     * @param percentiles the percentiles
     */
    public NominalPercentileDiscretizer(int... percentiles) {
        this.percentiles = percentiles;
    }

    // Binary search would be more efficient, however, array is expected to be small (<10 entries)
    private static int searchSorted(LinkedHashSet<Double> set, Number x) {
        int i = 0;
        for (Double current : set) {
            if (x.doubleValue() <= current) {
                break;
            }
            i++;
        }
        return i;
    }

    @Override
    public void fit(Serializable[] values) {
        double[] columnAsDouble = Stream.of(values).mapToDouble(v -> ((Number) v).doubleValue()).toArray();
        fittedPercentiles = new LinkedHashSet<>();
        Percentile percentile = new Percentile();
        for (int p : percentiles) {
            double evaluate = percentile.evaluate(columnAsDouble, p);
            fittedPercentiles.add(evaluate);
        }
    }

    @Override
    public DiscretizerRelation unApply(int value) {
        // TODO implement
        throw new IllegalStateException("Method not implemented");
    }

    @Override
    public Integer apply(Serializable o) {
        return searchSorted(fittedPercentiles, (Number) o);
    }
}
