package de.viadee.anchorj.tabular.discretizer;

/**
 * Discretizer mapping each value to its integer value
 */
public class OneToOneObjectDiscretizer extends AbstractDiscretizer<Object> {

    @Override
    public void fit(Object[] values) {
    }

    @Override
    public boolean isResultNumeric() {
        return false;
    }

    @Override
    public Object apply(Object o) {
        return o;
    }
}
