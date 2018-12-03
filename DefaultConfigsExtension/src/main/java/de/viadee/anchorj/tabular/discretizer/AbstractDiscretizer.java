package de.viadee.anchorj.tabular.discretizer;

import java.io.Serializable;
import java.util.function.Function;

public abstract class AbstractDiscretizer<R> implements Function<Object, R>, Serializable {

    public abstract void fit(Object[] values);

    public abstract boolean isResultNumeric();
}
