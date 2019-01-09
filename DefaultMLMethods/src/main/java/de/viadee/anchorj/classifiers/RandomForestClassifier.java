package de.viadee.anchorj.classifiers;

import smile.classification.RandomForest;

/**
 * Implementation of a random forest classifier
 */
public class RandomForestClassifier extends AbstractTextEnabledNumericClassifier {
    private static final long serialVersionUID = 5692525222756278360L;

    private final int nTrees;
    private RandomForest randomForest;

    /**
     * Instantiates a new Random forest classifier.
     *
     * @param nTrees the number of trees to create
     */
    public RandomForestClassifier(int nTrees) {
        this.nTrees = nTrees;
    }

    @Override
    public void fit(double[][] data, int[] labels) {
        randomForest = new RandomForest(data, labels, nTrees);
    }

    @Override
    public int[] predictMultiple(double[][] data) {
        if (randomForest == null)
            throw new RuntimeException("NumericClassifier has to be fitted first");
        return randomForest.predict(data);
    }

    @Override
    public int predict(double[] data) {
        if (randomForest == null)
            throw new RuntimeException("NumericClassifier has to be fitted first");
        return randomForest.predict(data);
    }
}
