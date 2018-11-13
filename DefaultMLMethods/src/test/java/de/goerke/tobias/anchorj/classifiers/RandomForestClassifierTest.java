package de.goerke.tobias.anchorj.classifiers;

import org.junit.jupiter.api.Test;
import smile.classification.RandomForest;
import smile.data.Attribute;
import smile.data.AttributeDataset;
import smile.data.NominalAttribute;
import smile.data.parser.ArffParser;
import smile.data.parser.DelimitedTextParser;
import smile.math.Math;
import smile.validation.LOOCV;

import static org.junit.jupiter.api.Assertions.assertTrue;

class RandomForestClassifierTest {

    /**
     * Test whether the imported library works as expected
     * <p>
     * Test of learn method, of class RandomForest.
     */
    @Test
    public void testRandomForestWeatherData() throws Exception {
        ArffParser arffParser = new ArffParser();
        arffParser.setResponseIndex(4);
        AttributeDataset weather = arffParser.parse(ClassLoader.getSystemClassLoader().getResourceAsStream("weather.nominal.arff"));
        double[][] x = weather.toArray(new double[weather.size()][]);
        int[] y = weather.toArray(new int[weather.size()]);

        int n = x.length;
        LOOCV loocv = new LOOCV(n);
        int error = 0;
        for (int i = 0; i < n; i++) {
            double[][] trainx = Math.slice(x, loocv.train[i]);
            int[] trainy = Math.slice(y, loocv.train[i]);

            RandomForest forest = new RandomForest(weather.attributes(), trainx, trainy, 100);
            if (y[loocv.test[i]] != forest.predict(x[loocv.test[i]]))
                error++;
        }
        assertTrue(error <= 7);
    }

    /**
     * Test whether the imported library works as expected
     * <p>
     * Test of learn method, of class RandomForest.
     */
    @Test
    public void testRandomForestUSPSNominal() throws Exception {
        DelimitedTextParser parser = new DelimitedTextParser();
        parser.setResponseIndex(new NominalAttribute("class"), 0);

        AttributeDataset train = parser.parse("USPS Train", ClassLoader.getSystemClassLoader().getResourceAsStream("zip.train"));
        AttributeDataset test = parser.parse("USPS Test", ClassLoader.getSystemClassLoader().getResourceAsStream("zip.test"));

        double[][] x = train.toArray(new double[train.size()][]);
        int[] y = train.toArray(new int[train.size()]);
        double[][] testx = test.toArray(new double[test.size()][]);
        int[] testy = test.toArray(new int[test.size()]);

        for (double[] xi : x) {
            for (int i = 0; i < xi.length; i++) {
                xi[i] = java.lang.Math.round(255 * (xi[i] + 1) / 2);
            }
        }

        for (double[] xi : testx) {
            for (int i = 0; i < xi.length; i++) {
                xi[i] = java.lang.Math.round(255 * (xi[i] + 1) / 2);
            }
        }

        Attribute[] attributes = new Attribute[256];
        String[] values = new String[attributes.length];
        for (int i = 0; i < attributes.length; i++) {
            values[i] = String.valueOf(i);
        }

        for (int i = 0; i < attributes.length; i++) {
            attributes[i] = new NominalAttribute("V" + i, values);
        }

        RandomForest forest = new RandomForest(attributes, x, y, 200);

        int error = 0;
        for (int i = 0; i < testx.length; i++) {
            if (forest.predict(testx[i]) != testy[i]) {
                error++;
            }
        }
        assertTrue(error <= 250);
    }
}