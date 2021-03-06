package de.viadee.xai.anchor.adapter.classifiers;

import smile.classification.LogisticRegression;

/**
 * Implements a logistic regression classifier.
 */
public class LogisticRegressionClassifier extends AbstractTextEnabledNumericClassifier {
    private static final long serialVersionUID = -1793494557059339703L;

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
