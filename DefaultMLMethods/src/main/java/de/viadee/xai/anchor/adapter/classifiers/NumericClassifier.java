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
}
