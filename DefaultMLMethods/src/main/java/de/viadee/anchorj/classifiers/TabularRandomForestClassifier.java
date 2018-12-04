package de.viadee.anchorj.classifiers;

import de.viadee.anchorj.tabular.TabularInstance;

import java.util.function.Function;
import java.util.stream.Stream;

/**
 * May be used to quickly train a model based on tabular data
 * <p>
 * Uses the {@link RandomForestClassifier} internally
 */
public class TabularRandomForestClassifier implements Function<TabularInstance, Integer> {
    private final RandomForestClassifier classifier;

    private TabularRandomForestClassifier(int nTrees) {
        this.classifier = new RandomForestClassifier(nTrees);
    }

    /**
     * Instantiates and fits new Random forest classifier using the tabular data
     *
     * @param nTrees      the number of trees to create
     * @param trainingSet the set to train the model on
     * @return the trained model
     */
    public static TabularRandomForestClassifier createAndFit(int nTrees, TabularInstance[] trainingSet) {
        final TabularRandomForestClassifier result = new TabularRandomForestClassifier(nTrees);

        double[][] doubleTrainingSet = new double[trainingSet.length][];
        for (int i = 0; i < trainingSet.length; i++) {
            final Object[] column = trainingSet[i].getInstance();
            double[] doubleColumn = new double[column.length];
            for (int j = 0; j < column.length; j++) {
                doubleColumn[j] = ((Number) column[j]).doubleValue();
            }
            doubleTrainingSet[i] = doubleColumn;
        }
        result.classifier.fit(doubleTrainingSet, Stream.of(trainingSet).mapToInt(TabularInstance::getOriginalLabel).toArray());

        return result;
    }

    @Override
    public Integer apply(TabularInstance instance) {
        double[] doubleColumn = new double[instance.getInstance().length];
        for (int j = 0; j < doubleColumn.length; j++)
            doubleColumn[j] = ((Number) instance.getInstance()[j]).doubleValue();
        return classifier.predict(doubleColumn);
    }
}
