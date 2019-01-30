package de.viadee.xai.anchor.adapter.model.h2o;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import hex.genmodel.easy.prediction.BinomialModelPrediction;
import hex.genmodel.easy.prediction.MultinomialModelPrediction;

public class H2oTabularNominalMojoClassifier<T extends TabularInstance> extends H2oTabularMojoClassifier<T> {
    private static final long serialVersionUID = -5889212341625113792L;

    public H2oTabularNominalMojoClassifier(InputStream mojoInputStream, List<String> sortedHeaderMapping) throws IOException {
        super(mojoInputStream, generateH2oNominalPredictionDiscretizer(), sortedHeaderMapping);
    }

    @Override
    public int predict(T instance) {
        return super.predict(instance);
    }

    /**
     * Handles {@link BinomialModelPrediction} and {@link MultinomialModelPrediction}.
     *
     * @return a function to extract the labelIndex value of the predictions
     */
    public static H2oTabularMojoClassifier.SerializableFunction generateH2oNominalPredictionDiscretizer() {
        return (prediction) -> {
            if (prediction instanceof BinomialModelPrediction) {
                return ((BinomialModelPrediction) prediction).labelIndex;
            } else if (prediction instanceof MultinomialModelPrediction) {
                return ((MultinomialModelPrediction) prediction).labelIndex;
            } else {
                throw new UnsupportedOperationException("Prediction of type: " + prediction.getClass().getSimpleName()
                        + "; not supported");
            }
        };
    }

}
