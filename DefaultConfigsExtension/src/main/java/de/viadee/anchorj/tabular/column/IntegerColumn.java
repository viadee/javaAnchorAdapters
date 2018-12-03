package de.viadee.anchorj.tabular.column;

import de.viadee.anchorj.tabular.discretizer.AbstractDiscretizer;
import de.viadee.anchorj.tabular.discretizer.UniqueValueDiscretizer;

public class IntegerColumn extends AbstractColumn<Integer> {

    public IntegerColumn(String name) {
        this(name, false, new UniqueValueDiscretizer());
    }

    public IntegerColumn(String name, boolean transformToInt) {
        this(name, transformToInt, new UniqueValueDiscretizer());
    }

    public IntegerColumn(String name, boolean transformToInt, AbstractDiscretizer<Integer> discretizer) {
        super(name);
        if (transformToInt)
            this.addTransformation(new StringToIntTransformator());
        this.setDiscretizer(discretizer);
    }
}
