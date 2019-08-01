package de.viadee.xai.anchor.adapter.classifiers;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import smile.classification.SoftClassifier;

import java.util.function.Function;
import java.util.stream.Stream;

/**
 * May be used to quickly train a SMILE model based on tabular data
 * <p>
 * CAUTION: SMILE is only able to handle double values. Thus, only discretized values are used for fitting and training.
 * This may cause anchors to return imprecise values, as the model is never passed all available values but only their
 * discretized instantiation.
 * Therefore, these models are only to be used for testing.
 * <p>
 * Note: There is a chance to learn and predict on transformed values. This is only possible for models where all
 * values are numeric / boolean
 * <p>
 * TODO make optional to operate on transformed / discretized values
 */
public abstract class AbstractTabularSmileClassifier implements Function<TabularInstance, Integer> {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractTabularSmileClassifier.class);

    private final boolean useDiscretizedValues;

    private SoftClassifier<double[]> classifier;


    /**
     * Creates the instance
     *
     * @param useDiscretizedValues if true, the model will be learned on transformed, i.e., original values.
     *                             Generally, this is to be preferred but not always possible.
     */
    AbstractTabularSmileClassifier(boolean useDiscretizedValues) {
        this.useDiscretizedValues = useDiscretizedValues;
    }

    private static Number toNumeric(Object obj) {
        Number result;
        if (Boolean.FALSE.equals(obj))
            result = 0D;
        else if (Boolean.TRUE.equals(obj))
            result = 1D;
        else if (obj instanceof Number)
            result = (Number) obj;
        else
            throw new IllegalArgumentException("Only numeric values may be used");

        return result;
    }

    private int getLabelValue(TabularInstance instance) {
        Object result = useDiscretizedValues ? instance.getDiscretizedLabel() : instance.getTransformedLabel();
        try {
            return toNumeric(result).intValue();
        } catch (IllegalArgumentException e) {
            // Is this good? For label try to use discretized instead
            if (!useDiscretizedValues)
                return toNumeric(instance.getDiscretizedLabel()).intValue();
            throw e;
        }
    }

    private Number[] getInstance(TabularInstance instance) {
        Object[] result = (useDiscretizedValues) ? instance.getInstance() : instance.getTransformedInstance();
        return Stream.of(result).map(AbstractTabularSmileClassifier::toNumeric).toArray(Number[]::new);
    }

    /**
     * Instantiates and fits new Random forest classifier using the tabular data
     *
     * @param trainingSet the set to train the model on
     */
    public void fit(TabularInstance[] trainingSet) {
        double[][] doubleTrainingSet = new double[trainingSet.length][];
        for (int i = 0; i < trainingSet.length; i++) {
            final Number[] column = getInstance(trainingSet[i]);
            double[] doubleColumn = new double[column.length];
            for (int j = 0; j < column.length; j++) {
                doubleColumn[j] = column[j].doubleValue();
            }
            doubleTrainingSet[i] = doubleColumn;
        }
        int[] labels = Stream.of(trainingSet).mapToInt(this::getLabelValue).toArray();

        this.classifier = fit(doubleTrainingSet, labels);
    }

    protected abstract SoftClassifier<double[]> fit(double[][] trainingSet, int[] labels);

    @Override
    public Integer apply(TabularInstance instance) {
        if (classifier == null)
            throw new IllegalStateException("The model must be fit in order to be used.");

        final double[] doubleColumn = new double[getInstance(instance).length];
        for (int j = 0; j < doubleColumn.length; j++)
            doubleColumn[j] = getInstance(instance)[j].doubleValue();

        return classifier.predict(doubleColumn);
    }
}
