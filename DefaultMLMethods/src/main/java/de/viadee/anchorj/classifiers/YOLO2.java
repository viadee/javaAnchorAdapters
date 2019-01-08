package de.viadee.anchorj.classifiers;

import org.deeplearning4j.nn.graph.ComputationGraph;
import org.deeplearning4j.nn.layers.objdetect.DetectedObject;
import org.deeplearning4j.nn.layers.objdetect.Yolo2OutputLayer;
import org.deeplearning4j.zoo.PretrainedType;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.api.preprocessor.ImagePreProcessingScaler;

import java.awt.*;
import java.io.IOException;
import java.util.*;
import java.util.List;

/**
 * A DL4J YOLO2 classifier.
 */
public class YOLO2 extends AbstractDL4JImageNetClassifier {
    private static final long serialVersionUID = -7178574918228681653L;

    private final String[] CLASSES = {"person", "bicycle", "car", "motorbike", "aeroplane", "bus", "train",
            "truck", "boat", "traffic light", "fire hydrant", "stop sign", "parking meter", "bench", "bird", "cat",
            "dog", "horse", "sheep", "cow", "elephant", "bear", "zebra", "giraffe", "backpack", "umbrella", "handbag",
            "tie", "suitcase", "frisbee", "skis", "snowboard", "sports ball", "kite", "baseball bat", "baseball glove",
            "skateboard", "surfboard", "tennis racket", "bottle", "wine glass", "cup", "fork", "knife", "spoon", "bowl",
            "banana", "apple", "sandwich", "orange", "broccoli", "carrot", "hot dog", "pizza", "donut", "cake", "chair",
            "sofa", "pottedplant", "bed", "diningtable", "toilet", "tvmonitor", "laptop", "mouse", "remote", "keyboard",
            "cell phone", "microwave", "oven", "toaster", "sink", "refrigerator", "book", "clock", "vase", "scissors",
            "teddy bear", "hair drier", "toothbrush"};

    /**
     * Instantiates the YOLO2 net.
     *
     * @param desiredDimension the desired dimension
     */
    public YOLO2(Dimension desiredDimension) {
        super(desiredDimension, createComputationGraph(), new ImagePreProcessingScaler(0, 1));
    }

    private static ComputationGraph createComputationGraph() {
        try {
            return (ComputationGraph) org.deeplearning4j.zoo.model.YOLO2.builder().build().initPretrained(PretrainedType.IMAGENET);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected List<Prediction> extractPredictions(INDArray array) {
        Yolo2OutputLayer outputLayer = (Yolo2OutputLayer) getComputationGraph().getOutputLayer(0);
        List<DetectedObject> detectedObjects = outputLayer.getPredictedObjects(array, 0.01);

        if (detectedObjects.isEmpty())
            return Collections.singletonList(new Prediction(-1, 0, "NO_OBJECT_DETECTED"));

        final Map<Integer, Double> classToProbability = new HashMap<>();
        for (DetectedObject detectedObject : detectedObjects) {
            Double existingProbability = classToProbability.getOrDefault(detectedObject.getPredictedClass(), 0D);
            if (detectedObject.getConfidence() > existingProbability)
                classToProbability.put(detectedObject.getPredictedClass(), detectedObject.getConfidence());
        }

        List<Prediction> result = new ArrayList<>();
        for (Map.Entry<Integer, Double> entry : classToProbability.entrySet())
            result.add(new Prediction(entry.getKey(), entry.getValue().floatValue(), CLASSES[entry.getKey()]));

        return result;
    }
}
