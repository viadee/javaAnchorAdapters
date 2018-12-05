package de.viadee.anchorj.tabular.util;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Provides methods to parse a CSV file
 */
public final class CSVReader {

    /**
     * Loads a CSV file from the resources path
     *
     * @param is   the input stream
     * @param trim if true, each on each cell String#trim will be called
     * @return a {@link Collection}, representing the rows, containing an array of string/columns
     * @throws IOException if the file cannot be loaded
     */
    public static Collection<String[]> readCSV(final InputStream is, final boolean trim) throws IOException {
        final CSVParser parser = new CSVParser(new InputStreamReader(is), CSVFormat.DEFAULT);

        final Collection<String[]> result = new ArrayList<>();
        for (CSVRecord csvRecord : parser) {
            final int colCount = csvRecord.size();
            final String[] columns = new String[colCount];
            for (int i = 0; i < colCount; i++) {
                String value = csvRecord.get(i);
                if (trim)
                    value = value.trim();
                columns[i] = value;
            }
            result.add(columns);
        }
        return result;
    }
}
