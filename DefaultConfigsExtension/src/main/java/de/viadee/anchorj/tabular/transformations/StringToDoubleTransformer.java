package de.viadee.anchorj.tabular.transformations;

import java.io.Serializable;
import java.util.stream.Stream;

/**
 * Transforms a string column to double values
 */
public class StringToDoubleTransformer extends StringToNumberTransformer implements Transformer {
    private static final long serialVersionUID = 2355994623681811418L;

    @Override
    public Double[] apply(Serializable[] strings) {
        return Stream.of(tryConvertToNumber(strings)).map(Number::doubleValue).toArray(Double[]::new);
    }

    @Override
    Number parse(String str) throws NumberFormatException {
        return Double.valueOf(str);
    }
}
