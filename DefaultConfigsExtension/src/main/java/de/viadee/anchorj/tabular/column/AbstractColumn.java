package de.viadee.anchorj.tabular.column;

import de.viadee.anchorj.tabular.discretizer.AbstractDiscretizer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.function.Function;

/**
 * Represents the type of a column - whether the contained data is categorical or nominal
 *
 * @param <T> the column's final value type
 */
public abstract class AbstractColumn<T> implements Serializable {

    private final String name;
    private List<Function<Object[], Object[]>> transformations = new ArrayList<>();
    private AbstractDiscretizer<T> discretizer;

    protected AbstractColumn(String name) {
        this(name, new ArrayList<>(), null);
    }

    protected AbstractColumn(String name, List<Function<Object[], Object[]>> transformations,
                             AbstractDiscretizer<T> discretizer) {
        this.name = name;
        this.transformations = transformations;
        this.discretizer = discretizer;
    }

    public AbstractColumn<T> addTransformation(Function<Object[], Object[]> transformation) {
        this.transformations.add(transformation);
        return this;
    }

    public String getName() {
        return name;
    }

    public Object[] transform(Object[] values) {
        if (transformations == null || transformations.isEmpty())
            return values;
        Object[] result = null;
        final ListIterator<Function<Object[], Object[]>> iter = transformations.listIterator();
        while (iter.hasNext()) {
            if (!iter.hasPrevious())
                result = iter.next().apply(values);
            else
                result = iter.next().apply(result);
        }
        return result;
    }

    public AbstractDiscretizer<T> getDiscretizer() {
        return discretizer;
    }

    public AbstractColumn<T> setDiscretizer(AbstractDiscretizer<T> discretizer) {
        this.discretizer = discretizer;
        return this;
    }

    public boolean isDoUse() {
        return true;
    }


}
