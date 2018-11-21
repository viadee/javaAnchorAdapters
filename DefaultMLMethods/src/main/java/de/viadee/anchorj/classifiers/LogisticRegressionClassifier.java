package de.viadee.anchorj.classifiers;

import smile.classification.LogisticRegression;

/**
 * Implements a logistic regression classifier.
 */
public class LogisticRegressionClassifier extends AbstractTextEnabledNumericClassifier {
    private LogisticRegression logisticRegression;

    @Override
    public void fit(double[][] data, int[] labels) {
        logisticRegression = new LogisticRegression(data, labels);
    }

    @Override
    public int[] predictMultiple(double[][] data) {
        if (logisticRegression == null)
            throw new RuntimeException("NumericClassifier has to be fitted first");
        return logisticRegression.predict(data);
    }

    @Override
    public int predict(double[] data) {
        if (logisticRegression == null)
            throw new RuntimeException("NumericClassifier has to be fitted first");
        return logisticRegression.predict(data);
    }

}
