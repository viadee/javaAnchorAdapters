package de.goerke.tobias.anchorj.classifiers;

import org.deeplearning4j.nn.graph.ComputationGraph;
import org.deeplearning4j.zoo.PretrainedType;
import org.nd4j.linalg.dataset.api.preprocessor.VGG16ImagePreProcessor;

import java.awt.*;
import java.io.IOException;

/**
 * A DL4J VGG16 classifier.
 */
public class VGG16 extends AbstractDL4JImageNetClassifier {
    private static final Dimension REQUIRED_DIMENSION = new Dimension(224, 224);

    /**
     * Instantiates the VGG16 net.
     */
    public VGG16() {
        super(REQUIRED_DIMENSION, createComputationGraph(), new VGG16ImagePreProcessor());
    }

    private static ComputationGraph createComputationGraph() {
        try {
            return (ComputationGraph) org.deeplearning4j.zoo.model.VGG16.builder().build().initPretrained(PretrainedType.IMAGENET);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
