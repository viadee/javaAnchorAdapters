package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;

/**
 * Transforms a string column to double values
 */
public class StringToDoubleTransformer extends StringToNumberTransformer implements Transformer {
    private static final long serialVersionUID = 2355994623681811418L;

    @Override
    public Double apply(Serializable strings) {
        return tryConvertToNumber(strings).doubleValue();
    }

    @Override
    Number parse(String str) throws NumberFormatException {
        return Double.valueOf(str);
    }
}
