package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;

/**
 * Asserts that, in fact, a column contains a numeric value and has not been forgotten to be transformed from a string
 */
public class AssertNumberTransformer implements Transformer {

    @Override
    public Serializable apply(Serializable serializable) {
        if (!(serializable instanceof Number)) {
            throw new IllegalArgumentException(String.format("[%s] is not of type number, as is required for this column",
                    serializable));
        }
        return serializable;
    }
}
