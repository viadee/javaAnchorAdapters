package de.viadee.xai.anchor.adapter.model;

import java.io.IOException;
import java.io.InputStream;

import org.deeplearning4j.nn.modelimport.keras.KerasModelImport;
import org.deeplearning4j.nn.modelimport.keras.exceptions.InvalidKerasConfigurationException;
import org.deeplearning4j.nn.modelimport.keras.exceptions.UnsupportedKerasConfigurationException;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;
import de.viadee.xai.anchor.algorithm.ClassificationFunction;
import de.viadee.xai.anchor.algorithm.DataInstance;

/**
 * TODO make public and implement
 *
 * @param <T>
 */
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
