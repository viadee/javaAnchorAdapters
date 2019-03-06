package de.viadee.xai.anchor.adapter.classifiers;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import smile.classification.SoftClassifier;

import java.util.function.Function;
import java.util.stream.Stream;

/**
 * May be used to quickly train a SMILE model based on tabular data
 */
public abstract class AbstractTabularSmileClassifier implements Function<TabularInstance, Integer> {
    private SoftClassifier<double[]> classifier;

    /**
     * Instantiates and fits new Random forest classifier using the tabular data
     *
     * @param trainingSet the set to train the model on
     */
    public void fit(TabularInstance[] trainingSet) {
        double[][] doubleTrainingSet = new double[trainingSet.length][];
        for (int i = 0; i < trainingSet.length; i++) {
            final Object[] column = trainingSet[i].getInstance();
            double[] doubleColumn = new double[column.length];
            for (int j = 0; j < column.length; j++) {
                doubleColumn[j] = ((Number) column[j]).doubleValue();
            }
            doubleTrainingSet[i] = doubleColumn;
        }
        int[] labels = Stream.of(trainingSet).mapToInt(TabularInstance::getDiscretizedLabel).toArray();
        this.classifier = fit(doubleTrainingSet, labels);
    }

    protected abstract SoftClassifier<double[]> fit(double[][] trainingSet, int[] labels);

    @Override
    public Integer apply(TabularInstance instance) {
        if (classifier == null)
            throw new IllegalStateException("The model must be fit in order to be used.");

        final double[] doubleColumn = new double[instance.getInstance().length];
        for (int j = 0; j < doubleColumn.length; j++)
            doubleColumn[j] = instance.getInstance()[j].doubleValue();
        return classifier.predict(doubleColumn);
    }
}
