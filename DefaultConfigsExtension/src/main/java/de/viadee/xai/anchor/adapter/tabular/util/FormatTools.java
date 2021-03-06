package de.viadee.xai.anchor.adapter.tabular.util;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.Locale;

/**
 * Provides means to format outputs
 */
public final class FormatTools {
    // Decimal Format is not thread safe, so use ThreadLocal
    private static final ThreadLocal<DecimalFormat> ROUND_TO_TWO_FORMATTER = ThreadLocal.withInitial(() -> {
        final DecimalFormat df = new DecimalFormat("#.##");
        df.setRoundingMode(RoundingMode.CEILING);
        df.setDecimalFormatSymbols(DecimalFormatSymbols.getInstance(Locale.US));
        return df;
    });

    /**
     * Rounds a number to closest two decimals
     *
     * @param number the number to format
     * @return the formatted output
     */
    public static String roundToTwo(Number number) {
        return ROUND_TO_TWO_FORMATTER.get().format(number);
    }


}
