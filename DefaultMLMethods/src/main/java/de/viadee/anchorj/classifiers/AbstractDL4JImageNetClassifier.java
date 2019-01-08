package de.viadee.anchorj.classifiers;

import de.viadee.anchorj.ClassificationFunction;
import de.viadee.anchorj.image.ImageInstance;
import org.datavec.image.loader.NativeImageLoader;
import org.deeplearning4j.nn.graph.ComputationGraph;
import org.deeplearning4j.zoo.util.imagenet.ImageNetLabels;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.api.preprocessor.DataNormalization;
import org.nd4j.linalg.factory.Nd4j;

import java.awt.*;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Abstract superclass for DL4J CNN classifiers
 */
abstract class AbstractDL4JImageNetClassifier implements ClassificationFunction<ImageInstance> {
    private static final long serialVersionUID = 880894269632132027L;

    private final Dimension requiredDimension;
    private final ComputationGraph computationGraph;
    private final NativeImageLoader nativeImageLoader;
    private final DataNormalization preprocessor;
    private final List<String> labels;

    /**
     * Instantiates a instance.
     *
     * @param requiredDimension the required dimension
     * @param computationGraph  the computation graph
     */
    AbstractDL4JImageNetClassifier(Dimension requiredDimension, ComputationGraph computationGraph) {
        this(requiredDimension, computationGraph, null);
    }

    /**
     * Instantiates a instance.
     *
     * @param requiredDimension the required dimension
     * @param computationGraph  the computation graph
     * @param preprocessor      the preprocessor
     */
    AbstractDL4JImageNetClassifier(Dimension requiredDimension, ComputationGraph computationGraph,
                                   DataNormalization preprocessor) {
        this(requiredDimension, computationGraph,
                new NativeImageLoader(requiredDimension.height, requiredDimension.width, 3), preprocessor);
    }

    /**
     * Instantiates a instance.
     *
     * @param requiredDimension the required dimension
     * @param computationGraph  the computation graph
     * @param nativeImageLoader the native image loader
     * @param preprocessor      the preprocessor
     */
    private AbstractDL4JImageNetClassifier(Dimension requiredDimension, ComputationGraph computationGraph,
                                           NativeImageLoader nativeImageLoader, DataNormalization preprocessor) {
        this.requiredDimension = requiredDimension;
        this.computationGraph = computationGraph;
        this.nativeImageLoader = nativeImageLoader;
        this.preprocessor = preprocessor;
        this.labels = loadLabelNames();
    }

    /**
     * Load label names list.
     * <p>
     * Make protected if needed.
     *
     * @return the list
     */
    private List<String> loadLabelNames() {
        // ImageNetLabels has no direct access to its internal list, so we have to read it one by one
        ImageNetLabels imageNetLabels = null;
        try {
            imageNetLabels = new ImageNetLabels();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        List<String> result = new ArrayList<>();
        boolean stop = false;
        int i = 0;
        while (!stop) {
            try {
                result.add(imageNetLabels.getLabel(i++));
            } catch (IndexOutOfBoundsException e) {
                stop = true;
            }
        }
        return result;
    }

    /**
     * Gets the label of a prediction
     *
     * @param prediction the prediction
     * @return the label
     */
    public String getLabel(int prediction) {
        return labels.get(prediction);
    }

    private INDArray loadImage(final ImageInstance instance) {
        // We could also remove this as the image loader would crop it accordingly, however, is not required for this
        // use case. Therefore, throw exception
        if (null != requiredDimension &&
                !new Dimension(instance.getInstance().getWidth(), instance.getInstance().getHeight()).equals(requiredDimension))
            throw new IllegalArgumentException("Must match required dimensions: " + requiredDimension);

        INDArray image;
        try {
            image = nativeImageLoader.asMatrix(instance.getInstance());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        if (null != preprocessor)
            preprocessor.transform(image);
        return image;
    }

    /**
     * Predicts the label and offers alternative predictions
     *
     * @param imageInstance  the imageInstance
     * @param maxPredictions the maximum number of predictions to obtain
     * @return a collection of {@link Prediction}s
     */
    public List<Prediction> predict(ImageInstance imageInstance, int maxPredictions) {
        final INDArray prediction = computationGraph.outputSingle(false, loadImage(imageInstance));
        List<Prediction> predictions = extractPredictions(prediction);

        return predictions.stream().sorted(Comparator.comparingDouble(Prediction::getProbability).reversed())
                .collect(Collectors.toList());
    }

    /**
     * Extract predictions list containing {@link Prediction} elements for an image.
     *
     * @param array the array
     * @return the list
     */
    protected List<Prediction> extractPredictions(INDArray array) {
        final List<Prediction> result = new ArrayList<>();

        int i = 0;
        for (int batch = 0; batch < array.size(0); batch++) {
            INDArray currentBatch = array.getRow(batch).dup();
            double previousProbability = 1;
            while (previousProbability > 0) {
                int label = Nd4j.argMax(currentBatch, 1).getInt(0, 0);
                float probability = currentBatch.getFloat(batch, label);
                currentBatch.putScalar(0, label, 0);
                result.add(new Prediction(label, probability, labels.get(label)));
                i++;
                previousProbability = probability;
            }
        }
        return result;
    }

    /**
     * Gets computation graph.
     *
     * @return the computation graph
     */
    protected ComputationGraph getComputationGraph() {
        return computationGraph;
    }

    @Override
    public int predict(ImageInstance instance) {
        return predict(instance, 10).get(0).label;
    }

    /**
     * Represents a prediction result for one specific label, includes its probability and the readable label
     */
    public static final class Prediction {
        private final int label;
        private final float probability;
        private final String strLabel;

        /**
         * Instantiates a new Prediction.
         *
         * @param label       the label
         * @param probability the probability
         * @param strLabel    the str label
         */
        Prediction(int label, float probability, String strLabel) {
            this.label = label;
            this.probability = probability;
            this.strLabel = strLabel;
        }

        /**
         * Gets label.
         *
         * @return the label
         */
        public int getLabel() {
            return label;
        }

        /**
         * Gets probability.
         *
         * @return the probability
         */
        public float getProbability() {
            return probability;
        }

        /**
         * Gets str label.
         *
         * @return the str label
         */
        public String getStrLabel() {
            return strLabel;
        }
    }
}
