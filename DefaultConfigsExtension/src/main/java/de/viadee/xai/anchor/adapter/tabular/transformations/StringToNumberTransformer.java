package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;

/**
 * Used to convert object or string values to arbitrary numbers.
 * See e.g. {@link StringToIntTransformer} and {@link StringToDoubleTransformer} for specific implementations
 */
public abstract class StringToNumberTransformer {

    /**
     * @param str the string to parse
     * @return the parsed string or an Exception
     * @throws NumberFormatException if its not possible to parse
     */
    abstract Number parse(String str) throws NumberFormatException;

    /**
     * Converts values to numbers
     *
     * @param value the value to be transformed
     * @return the resultant numbers
     * @throws NumberFormatException if an object could not be read
     */
    protected Number tryConvertToNumber(Serializable value) throws NumberFormatException {
        try {
            if (value == null)
                throw new NullPointerException("Value may not be null. " +
                        "Consider using a ReplaceNullTransformer before discretization.");

            if (value instanceof Number) {
                return (Number) value;
            } else if (value instanceof String) {
                return parse(value.toString().trim());
            } else if (value instanceof Throwable) {
                // If an exception in the stream is thrown it is put into the stream itself?!
                throw (Throwable) value;
            } else {
                throw new NumberFormatException("Unknown data type definition");
            }
        } catch (Throwable e) {
            if (value instanceof RuntimeException) {
                // If an exception in the stream is thrown it is put into the stream itself?!
                throw (RuntimeException) value;
            }
            throw new NumberFormatException("Could not convert " + value + " to number");
        }
    }

    /**
     * Converts values to numbers
     *
     * @param values the values to be transformed
     * @return the resultant numbers
     * @throws NumberFormatException if an object could not be read
     */
    protected Number[] tryConvertToNumber(Serializable[] values) throws NumberFormatException {
        Number[] result = new Number[values.length];
        for (int i = 0; i < values.length; i++) {
            result[i] = tryConvertToNumber(result[i]);
        }
        return result;
    }
}
