package Initializer;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import de.viadee.xai.anchor.adapter.tabular.util.CSVReader;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Iterator;

public class TabularInitializer {

    static AnchorTabular createTabularDefinition(String path, int indexTarget, boolean ignoreFirst) {
        InputStream dataStream = ClassLoader.getSystemResourceAsStream(path);
        if (dataStream == null)
            throw new RuntimeException("Could not load data");

        Collection<String[]> dataframe;

        try {
            dataframe = CSVReader.readCSV(dataStream, false);
        } catch (IOException e) {
            throw new RuntimeException("Could not read csv");
        }

        AnchorTabular.Builder anchorTabular = new AnchorTabular.Builder();
        anchorTabular.setDoBalance(false);

        Iterator<String[]> iterator = dataframe.iterator();
        String[] firstRow = iterator.next();
        if (ignoreFirst) {
            dataframe.remove(firstRow);
        }

        int[] datatypes = getColumnDataTypes(dataframe);

        for (int i = 0; i < datatypes.length; i++) {

            if (i == indexTarget) {
                anchorTabular.addTargetColumn(IntegerColumn.fromStringInput(firstRow[i]));
            } else {
                switch (datatypes[i]) {
                    case 0:
                        anchorTabular.addColumn(IntegerColumn.fromStringInput(firstRow[i], -1, null, null));
                        break;
                    case 1:
                        anchorTabular.addColumn(DoubleColumn.fromStringInput(firstRow[i], -1, 5));
                        break;
                    case 2:
                        anchorTabular.addColumn(new StringColumn(firstRow[i]));
                        break;
                }
            }
        }

        return anchorTabular.build(dataframe, ignoreFirst);
    }

    static int[] getColumnDataTypes(Collection<String[]> dataFrame) {

        int[] datatypes = new int[dataFrame.iterator().next().length];

        for (String[] row : dataFrame) {

            for (int i = 0; i < row.length; i++) {
                // if int 0; if double 1; if String 2
                if (row[i].matches("^-?\\d+$") && datatypes[i] <= 0) {
                    datatypes[i] = 0;
                } else if (row[i].matches("^-?\\d*\\.\\d+$") && datatypes[i] <= 1) {
                    datatypes[i] = 1;
                } else if (row[i].matches(".*[a-zA-Z]+.*") && !row[i].equals("")) {
                    datatypes[i] = 2;
                }
            }
            System.out.println();
        }
        return datatypes;
    }

}
