package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;

/**
 * Transforms a string column to integer values
 */
public class StringToIntTransformer extends StringToNumberTransformer implements Transformer {
    private static final long serialVersionUID = 4475610400175110033L;

    @Override
    public Integer apply(Serializable string) {
        return tryConvertToNumber(string).intValue();
    }

    @Override
    Number parse(String str) throws NumberFormatException {
        return Integer.valueOf(str);
    }
}
