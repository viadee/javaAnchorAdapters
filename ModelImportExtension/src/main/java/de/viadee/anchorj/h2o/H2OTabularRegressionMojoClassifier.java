package de.viadee.anchorj.h2o;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import de.viadee.anchorj.tabular.TabularInstance;
import hex.genmodel.easy.prediction.RegressionModelPrediction;

public class H2OTabularRegressionMojoClassifier<T extends TabularInstance> extends H2oTabularMojoClassifier<T> {
    private static final long serialVersionUID = -2625940249220401118L;

    public H2OTabularRegressionMojoClassifier(InputStream mojoInputStream, SerializableFunction predictionDiscretizer, List<String> sortedHeaderMapping) throws IOException {
        super(mojoInputStream, predictionDiscretizer, sortedHeaderMapping);
    }

    /**
     * Handles {@link RegressionModelPrediction}.
     *
     * @return a function to extract the regression value and converts it to int.
     */
    public static H2oTabularMojoClassifier.SerializableFunction generateH2oRegressionPredictionDiscretizer() {
        return (prediction) -> {
            if (prediction instanceof RegressionModelPrediction) {
                return (int) (((RegressionModelPrediction) prediction).value);
            } else {
                throw new UnsupportedOperationException("Prediction of type: " + prediction.getClass().getSimpleName()
                        + "; not supported");
            }
        };
    }

}
