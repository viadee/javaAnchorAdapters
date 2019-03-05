package de.viadee.xai.anchor.adapter.classifiers;

import java.io.Serializable;

/**
 * Interface marking classifiers that are able to predict regression problems
 */
interface NumericClassifier extends Serializable {
    /**
     * Fit/train the model.
     *
     * @param data   the data
     * @param labels the labels
     */
    void fit(double[][] data, int[] labels);

    /**
     * Predict multiple instances
     *
     * @param data the data
     * @return the int array containing single predictions
     */
    int[] predictMultiple(double[][] data);

    /**
     * Predict a single instance
     *
     * @param data the data
     * @return the int
     */
    int predict(double[] data);

    /**
     * Calculate prediction accuracy.
     *
     * @param predictions the predictions
     * @param actual      the actual
     * @return the double
     */
    static double calculatePredictionAccuracy(int[] predictions, int[] actual) {
        if (predictions.length != actual.length)
            throw new IllegalArgumentException("Prediction and data set must be of same length");
        int correctPredictions = 0;
        for (int i = 0; i < predictions.length; i++) {
            if (predictions[i] == actual[i])
                correctPredictions++;
        }
        return ((double) correctPredictions) / predictions.length;
    }

}
