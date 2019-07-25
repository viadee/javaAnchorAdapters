package de.viadee.xai.anchor.adapter.classifiers;

import smile.classification.RandomForest;
import smile.classification.SoftClassifier;

/**
 * May be used to quickly train a model based on tabular data
 */
public class TabularRandomForestClassifier extends AbstractTabularSmileClassifier {
    private final int nTrees;

    /**
     * Instantiates the classifier
     *
     * @param nTrees               the number of trees to use
     * @param useDiscretizedValues if true, the model will be learned on transformed, i.e., original values.
     *                             Generally, this is to be preferred but not always possible.
     */
    public TabularRandomForestClassifier(int nTrees, boolean useDiscretizedValues) {
        super(useDiscretizedValues);
        this.nTrees = nTrees;
    }

    @Override
    protected SoftClassifier<double[]> fit(double[][] trainingSet, int[] labels) {
        return new RandomForest(trainingSet, labels, nTrees);
    }
}
