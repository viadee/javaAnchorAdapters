package de.viadee.anchorj.tabular.transformations;

/**
 * Used to convert object or string values to arbitrary numbers.
 * See e.g. {@link StringToIntTransformer} and {@link StringToDoubleTransformer} for specific implementations
 */
abstract class StringToNumberTransformer {

    private static Number parse(String str) {
        Number number;
        try {
            number = Float.parseFloat(str);
        } catch (NumberFormatException e) {
            try {
                number = Double.parseDouble(str);
            } catch (NumberFormatException e1) {
                try {
                    number = Integer.parseInt(str);
                } catch (NumberFormatException e2) {
                    try {
                        number = Long.parseLong(str);
                    } catch (NumberFormatException e3) {
                        throw e3;
                    }
                }
            }
        }
        return number;
    }


    /**
     * Converts values to numbers
     *
     * @param values the values to be transformed
     * @return the resultant numbers
     * @throws NumberFormatException if an object could not be read
     */
    protected static Number[] tryConvertToNumber(Object[] values) throws NumberFormatException {
        Number[] result = new Number[values.length];
        for (int i = 0; i < values.length; i++) {
            try {
                if (values[i] == null)
                    throw new NullPointerException("Value may not be null. " +
                            "Consider using a ReplaceNullTransformer before discretization.");

                if (values[i] instanceof Number) {
                    result[i] = (Number) values[i];
                } else if (values[i] instanceof String) {
                    String value = (String) values[i];
                    result[i] = parse(value.trim());
                } else if (values[i] instanceof Throwable) {
                    // If an exception in the stream is thrown it is put into the stream itself?!
                    throw (Throwable) values[i];
                } else {
                    throw new NumberFormatException("Unknown data type definition");
                }
            } catch (Throwable e) {
                if (values[i] instanceof RuntimeException) {
                    // If an exception in the stream is thrown it is put into the stream itself?!
                    throw (RuntimeException) values[i];
                }
                throw new NumberFormatException("Could not convert " + values[i] + " to number");
            }
        }
        return result;
    }
}