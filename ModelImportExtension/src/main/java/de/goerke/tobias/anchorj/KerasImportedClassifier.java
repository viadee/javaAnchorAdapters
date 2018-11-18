package de.goerke.tobias.anchorj;

import org.deeplearning4j.nn.modelimport.keras.KerasModelImport;
import org.deeplearning4j.nn.modelimport.keras.exceptions.InvalidKerasConfigurationException;
import org.deeplearning4j.nn.modelimport.keras.exceptions.UnsupportedKerasConfigurationException;
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork;

import java.io.IOException;
import java.io.InputStream;

// TODO make public & implement
class KerasImportedClassifier<T extends DataInstance<?>> implements ClassificationFunction<T> {

    public KerasImportedClassifier(final InputStream modelInputStream) throws InvalidKerasConfigurationException,
            IOException, UnsupportedKerasConfigurationException {
        MultiLayerNetwork multiLayerNetwork = KerasModelImport.importKerasSequentialModelAndWeights(modelInputStream);
    }

    @Override
    public int predict(T instance) {
        return 0;
    }
}
