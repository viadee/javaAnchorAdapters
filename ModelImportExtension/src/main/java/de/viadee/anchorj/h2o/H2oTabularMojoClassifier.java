package de.viadee.anchorj.h2o;

import de.viadee.anchorj.ClassificationFunction;
import de.viadee.anchorj.tabular.TabularInstance;
import hex.genmodel.ModelMojoReader;
import hex.genmodel.MojoModel;
import hex.genmodel.MojoReaderBackend;
import hex.genmodel.MojoReaderBackendFactory;
import hex.genmodel.easy.EasyPredictModelWrapper;
import hex.genmodel.easy.RowData;
import hex.genmodel.easy.prediction.AbstractPrediction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.List;
import java.util.function.Function;

@SuppressWarnings("WeakerAccess")
public class H2oTabularMojoClassifier<T extends TabularInstance> implements ClassificationFunction<T> {
    private static final long serialVersionUID = -5889212341625113792L;

    private static final Logger LOG = LoggerFactory.getLogger(H2oTabularMojoClassifier.class);

    private final EasyPredictModelWrapper modelWrapper;
    private final SerializableFunction predictionDiscretizer;
    private final String[] sortedColumnNames;

    public H2oTabularMojoClassifier(
            InputStream mojoInputStream,
            SerializableFunction predictionDiscretizer,
            List<String> sortedHeaderMapping) throws IOException {
        this.sortedColumnNames = sortedHeaderMapping.toArray(new String[0]);
        this.predictionDiscretizer = predictionDiscretizer;

        final MojoReaderBackend reader = MojoReaderBackendFactory.createReaderBackend(mojoInputStream,
                MojoReaderBackendFactory.CachingStrategy.MEMORY);
        final MojoModel model = ModelMojoReader.readFrom(reader);

        this.modelWrapper = new EasyPredictModelWrapper(model);
    }

    @Override
    public int predict(final T instance) {
        Serializable[] instanceValues;
        if (instance.getTransformedInstance() != null) {
            instanceValues = instance.getTransformedInstance();
        } else {
            if (LOG.isDebugEnabled()) {
                LOG.debug("Trying to predict with h2o model and the discretized " +
                        "instance values since the original instance is null");
            }
            instanceValues = instance.getInstance();
        }

        RowData row = new RowData();
        for (int i = 0; i < sortedColumnNames.length; i++) {
            row.put(sortedColumnNames[i], instanceValues[i]);
        }

        try {
            AbstractPrediction prediction = this.getModelWrapper().predict(row);
            return predictionDiscretizer.apply(prediction);
        } catch (hex.genmodel.easy.exception.PredictException e) {
            throw new H2oPredictException(e);
        }
    }

    public EasyPredictModelWrapper getModelWrapper() {
        return modelWrapper;
    }

    public interface SerializableFunction extends Function<AbstractPrediction, Integer>, Serializable {
    }

}
