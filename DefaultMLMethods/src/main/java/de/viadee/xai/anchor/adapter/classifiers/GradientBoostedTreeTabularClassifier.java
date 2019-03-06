package de.viadee.xai.anchor.adapter.classifiers;

import smile.classification.GradientTreeBoost;
import smile.classification.RandomForest;
import smile.classification.SoftClassifier;

/**
 * May be used to quickly train a model based on tabular data
 */
public class GradientBoostedTreeTabularClassifier extends AbstractTabularSmileClassifier {
    private final int nTrees;

    /**
     * Instantiates the classifier
     *
     * @param nTrees the number of trees to use
     */
    public GradientBoostedTreeTabularClassifier(int nTrees) {
        this.nTrees = nTrees;
    }

    @Override
    protected SoftClassifier<double[]> fit(double[][] trainingSet, int[] labels) {
        return new GradientTreeBoost(trainingSet, labels, nTrees);
    }
}
