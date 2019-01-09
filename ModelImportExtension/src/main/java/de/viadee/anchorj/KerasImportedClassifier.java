package de.viadee.anchorj;

import java.io.IOException;
import java.io.InputStream;

import org.deeplearning4j.nn.modelimport.keras.KerasModelImport;
import org.deeplearning4j.nn.modelimport.keras.exceptions.InvalidKerasConfigurationException;
import org.deeplearning4j.nn.modelimport.keras.exceptions.UnsupportedKerasConfigurationException;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;

// TODO make public & implement
public class KerasImportedClassifier<T extends DataInstance<?>> implements ClassificationFunction<T> {
    private static final long serialVersionUID = -2516330397459812978L;

    public KerasImportedClassifier(final InputStream modelInputStream) throws InvalidKerasConfigurationException,
            IOException, UnsupportedKerasConfigurationException {
        MultiLayerNetwork multiLayerNetwork = KerasModelImport.importKerasSequentialModelAndWeights(modelInputStream);
    }

    @Override
    public int predict(T instance) {
        return 0;
    }
}
