package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;

/**
 * Transforms TRUE/FALSE values to Boolean types
 */
public class StringToBooleanTransformer implements Transformer{

    @Override
    public Boolean apply(Serializable serializable) {
        return tryConvertToBoolean(serializable);
    }

    private boolean tryConvertToBoolean(Serializable serializable) throws NumberFormatException {
        String value = serializable.toString().toLowerCase();
        if (!Boolean.TRUE.toString().equals(value) && !Boolean.FALSE.toString().equals(value))
            throw new IllegalArgumentException("Value is not a boolean");
        return Boolean.valueOf(String.valueOf(serializable));
    }
}
