package de.viadee.anchorj.tabular.column;

import java.util.function.Function;
import java.util.stream.Stream;

public class StringToIntTransformator implements Function<Object[], Object[]> {

    private static Number parse(String str) {
        Number number = null;
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

    protected static Number tryConvertToNumber(Object value) {
        return tryConvertToNumber(new Object[]{value})[0];
    }

    protected static Number[] tryConvertToNumber(Object[] values) {
        Number[] result = new Number[values.length];
        for (int i = 0; i < values.length; i++) {
            if (values[i] instanceof Number) {
                result[i] = (Number) values[i];
                continue;
            } else if (values[i] instanceof String) {
                try {
                    result[i] = parse((String) values[i]);
                    continue;
                } catch (Exception ignored) {
                }
            }
            throw new IllegalArgumentException("Could not convert " + values[i] + " to number");
        }
        return result;
    }

    @Override
    public Integer[] apply(Object[] strings) {
        return Stream.of(tryConvertToNumber(strings)).mapToInt(Number::intValue).boxed().toArray(Integer[]::new);
    }
}