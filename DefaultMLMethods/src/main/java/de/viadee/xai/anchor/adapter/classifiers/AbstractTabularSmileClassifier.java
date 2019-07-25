package de.viadee.xai.anchor.adapter.classifiers;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.discretizer.CategoricalDiscretizationOrigin;
import de.viadee.xai.anchor.adapter.tabular.discretizer.DiscretizationOrigin;
import de.viadee.xai.anchor.adapter.tabular.discretizer.impl.UniqueValueDiscretizer;
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
 * TODO make optional to operate on transformed / discretized values
 */
public abstract class AbstractTabularSmileClassifier implements Function<TabularInstance, Integer> {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractTabularSmileClassifier.class);

    private SoftClassifier<double[]> classifier;
    private UniqueValueDiscretizer labelDiscretizer;

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

        if (Stream.of(trainingSet).anyMatch(i -> ((Number) i.getDiscretizedLabel()).doubleValue() % 1 != 0)) {
            LOGGER.warn("There are labels having non-int numeric values. UniqueValueDiscretizer will be applied");
            labelDiscretizer = new UniqueValueDiscretizer();
            labelDiscretizer.fit(Stream.of(trainingSet).map(t -> (Number) t.getDiscretizedLabel()).toArray(Number[]::new));
        }

        int[] labels;
        if (labelDiscretizer == null) {
            // Only int values
            labels = Stream.of(trainingSet).mapToInt(t -> t.getDiscretizedLabel().intValue()).toArray();
        } else {
            final Double[] tmpLabels = labelDiscretizer.apply(Stream.of(trainingSet).map(t -> (Number) t.getDiscretizedLabel()).toArray(Number[]::new));
            labels = Stream.of(tmpLabels).mapToInt(Double::intValue).toArray();
        }

        //double[] labels = Stream.of(trainingSet).mapToInt(TabularInstance::getDiscretizedLabel).toArray();
        this.classifier = fit(doubleTrainingSet, labels);
    }

    protected abstract SoftClassifier<double[]> fit(double[][] trainingSet, int[] labels);

    @Override
    public Integer apply(TabularInstance instance) {
        if (classifier == null)
            throw new IllegalStateException("The model must be fit in order to be used.");

        final double[] doubleColumn = new double[instance.getInstance().length];
        for (int j = 0; j < doubleColumn.length; j++)
            doubleColumn[j] = instance.getInstance()[j];

        int prediction = classifier.predict(doubleColumn);

        if (labelDiscretizer == null) {
            return prediction;
        } else {
            DiscretizationOrigin discretizationOrigin = labelDiscretizer.getTransition((double) prediction).getDiscretizationOrigin();
            return ((Number) ((CategoricalDiscretizationOrigin) discretizationOrigin).getValue()).intValue();
        }
    }
}
