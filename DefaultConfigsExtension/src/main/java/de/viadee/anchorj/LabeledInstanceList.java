package de.viadee.anchorj;

import java.io.Serializable;
import java.util.AbstractList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Provides a way to store multiple instances and their corresponding labels.
 * <p>
 * TODO remove eventually
 *
 * @param <T> Type of the hold {@link DataInstance}
 */
@Deprecated
public abstract class LabeledInstanceList<T extends DataInstance<?>> extends AbstractList<T> implements Serializable {
    private static final long serialVersionUID = 6638791181736377145L;

    protected final T[] dataInstances;
    protected final int[] labels;
    protected final int featureCount;

    /**
     * Constructs the instance
     *
     * @param dataInstances the data instances
     * @param labels        the labels for each instance
     */
    public LabeledInstanceList(final T[] dataInstances, final int[] labels) {
        if (dataInstances.length < 1)
            throw new IllegalArgumentException("The list must at least contain one element");
        if (labels != null && labels.length != dataInstances.length)
            throw new IllegalArgumentException("There must be a label for each instance");
        // Not all instances must have the same feature length. Look at e.g. text instances where each
        // instance is a text of variable length. Therefore, no validation here.
        this.dataInstances = dataInstances;
        this.labels = labels;
        this.featureCount = dataInstances[0].getFeatureCount();
    }

    /**
     * @return a UnmodifiableList of the contained instances
     */
    public List<T> getInstances() {
        return Collections.unmodifiableList(Arrays.asList(dataInstances));
    }

    /**
     * @return an UnmodifiableList of the contained labels
     */
    public List<Integer> getLabels() {
        if (labels == null)
            return Collections.emptyList();
        return IntStream.of(labels).boxed().collect(Collectors.collectingAndThen(
                Collectors.toList(), Collections::unmodifiableList));
    }

    @Override
    public T get(int i) {
        return dataInstances[i];
    }

    @Override
    public int size() {
        return dataInstances.length;
    }

    /**
     * @return the count of features each element has
     */
    public int getFeatureCount() {
        return featureCount;
    }

    /**
     * Tests a classification function by comparing its predictions vs. the actual labels
     *
     * @param classificationFunction the classification function which accuracy shall be tested
     * @return the accuracy
     */
    public double calculatePredictionAccuracy(final ClassificationFunction<T> classificationFunction) {
        if (labels == null)
            throw new IllegalArgumentException("Labels need to be included in order to compute accuracy.");
        int correctCount = 0;
        for (int i = 0; i < labels.length; i++) {
            final int prediction = classificationFunction.predict(dataInstances[i]);
            if (prediction == labels[i])
                correctCount++;
        }
        return correctCount / (double) labels.length;
    }
}
