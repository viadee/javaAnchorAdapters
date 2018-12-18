package de.viadee.anchorj.tabular.transformations;

import java.io.Serializable;
import java.util.stream.Stream;

/**
 * Transforms a string column to integer values
 */
public class StringToIntTransformer extends StringToNumberTransformer implements Transformer {
    private static final long serialVersionUID = 4475610400175110033L;

    @Override
    public Integer[] apply(Serializable[] strings) {
        return Stream.of(tryConvertToNumber(strings)).map(Number::intValue).toArray(Integer[]::new);
    }

    @Override
    Number parse(String str) throws NumberFormatException {
        return Integer.valueOf(str);
    }
}
