package de.viadee.anchorj.tabular.transformations;

import java.util.stream.Stream;

/**
 * Transforms a string column to integer values
 */
public class StringToIntTransformer extends StringToNumberTransformer implements Transformer {

    @Override
    public Integer[] apply(Object[] strings) {
        return Stream.of(tryConvertToNumber(strings)).map(Number::intValue).toArray(Integer[]::new);
    }
}
