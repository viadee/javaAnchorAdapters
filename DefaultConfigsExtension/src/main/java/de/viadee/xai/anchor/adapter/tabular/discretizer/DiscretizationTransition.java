package de.viadee.xai.anchor.adapter.tabular.discretizer;

/**
 * The relation between original and discretized value
 */
public final class DiscretizationTransition {
    private final DiscretizationOrigin discretizationOrigin;
    private final int discretizedValue;

    /**
     * Constructs the instance
     *
     * @param discretizationOrigin the discretizationOrigin
     * @param discretizedValue     the discretizedValue
     */
    public DiscretizationTransition(DiscretizationOrigin discretizationOrigin,
                                    int discretizedValue) {
        this.discretizationOrigin = discretizationOrigin;
        this.discretizedValue = discretizedValue;
    }

    /**
     * @return the discretization origin
     */
    public DiscretizationOrigin getDiscretizationOrigin() {
        return discretizationOrigin;
    }

    /**
     * @return the discretized value
     */
    public int getDiscretizedValue() {
        return discretizedValue;
    }

    @Override
    public String toString() {
        // TODO setup correct output or create new method
        return "DiscretizationTransition{" +
                "discretizationOrigin=" + discretizationOrigin +
                ", discretizedValue=" + discretizedValue +
                '}';
    }
}
