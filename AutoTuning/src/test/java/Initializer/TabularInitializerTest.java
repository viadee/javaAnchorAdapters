package Initializer;

import de.viadee.xai.anchor.adapter.tabular.util.CSVReader;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class TabularInitializerTest {

    @Test
    public void createTabularTrainingDefinition() {

        // Given
        String path = "train.csv";

        // Then
        TabularInitializer.createTabularDefinition(path, 0, true);

    }

    @Test
    public void testGetColumnDatatype() {

        // Given
        String path = "train.csv";
        InputStream trainingDataStream = ClassLoader.getSystemResourceAsStream(path);
        Collection<String[]> strings = null;
        try {
            strings = CSVReader.readCSV(trainingDataStream, false);
        } catch (IOException e) {

        }

        // When
        int[] result = TabularInitializer.getColumnDataTypes(strings);

    }

    @Test
    public void testEmptyRowDatatypes() {
        // Given
        String[] row = {};
        Collection<String[]> dataframe = new ArrayList<>();
        dataframe.add(row);

        // When
        int[] result = TabularInitializer.getColumnDataTypes(dataframe);

        // Then
        Assert.assertTrue(result.length == 0);
    }

    @Test
    public void testIntRowDatatypes() {
        // Given
        String[] row = {"1", "2", "3", "4"};
        Collection<String[]> dataframe = new ArrayList<>();
        dataframe.add(row);

        // When
        int[] result = TabularInitializer.getColumnDataTypes(dataframe);
        int[] test ={0, 0, 0, 0};

        // Then
        Assert.assertTrue(Arrays.equals(result,test));
    }

    @Test
    public void testAllDatatypes() {
        // Given
        String[] row = new String[]{"1", "Hallo", "Test123.3", "3.12"};
        Collection<String[]> dataframe = new ArrayList<>();
        dataframe.add(row);

        // When
        int[] result = TabularInitializer.getColumnDataTypes(dataframe);
        int[] test ={0, 2, 2, 1};

        // Then
        Assert.assertTrue(Arrays.equals(result,test));
    }
}