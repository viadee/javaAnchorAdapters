package de.viadee.xai.anchor.adapter.tabular.discretizer;

import java.io.Serializable;

public class BooleanDiscretizer implements Discretizer {

    @Override
    public void fit(Serializable[] values) {

    }

    @Override
    public DiscretizerRelation unApply(int value) {
        return new DiscretizerRelation(value, (value == 1));
    }

    @Override
    public Integer apply(Serializable serializable) {
        String value = serializable.toString().toLowerCase();
        if (!Boolean.TRUE.toString().equals(value) && !Boolean.FALSE.toString().equals(value))
            throw new RuntimeException("Value is not a boolean");
        boolean bool = Boolean.valueOf(String.valueOf(serializable));
        return bool ? 1 : 0;
    }
}
