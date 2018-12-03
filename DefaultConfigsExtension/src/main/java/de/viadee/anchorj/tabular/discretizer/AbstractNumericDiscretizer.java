package de.viadee.anchorj.tabular.discretizer;

public abstract class AbstractNumericDiscretizer extends AbstractDiscretizer<Integer> {

    @Override
    public boolean isResultNumeric() {
        return true;
    }
}
