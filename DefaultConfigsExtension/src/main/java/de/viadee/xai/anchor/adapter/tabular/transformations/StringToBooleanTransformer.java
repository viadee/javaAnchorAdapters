package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;

/**
 * Transforms TRUE/FALSE or numeric values to Boolean types
 * <p>
 * Numeric values are converted to FALSE, when they equal 0, otherwise they are true
 */
public class StringToBooleanTransformer implements Transformer {

    private static boolean isNumeric(String str) {
        try {
            Double.parseDouble(str);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }

    @Override
    public Boolean apply(Serializable serializable) {
        return tryConvertToBoolean(serializable);
    }

    private boolean tryConvertToBoolean(Serializable serializable) throws NumberFormatException {
        final String value = serializable.toString().toLowerCase();
        Double numeric;
        try {
            numeric = Double.parseDouble(value);
        } catch (NumberFormatException e) {
            numeric = null;
        }
        if ((!Boolean.TRUE.toString().equals(value) && !Boolean.FALSE.toString().equals(value)) &&
                numeric == null)
            throw new IllegalArgumentException("Value is not a boolean");
        if (numeric != null)
            return numeric != 0D;
        return Boolean.valueOf(String.valueOf(serializable));
    }
}
