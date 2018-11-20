package de.goerke.tobias.anchorj;

import de.goerke.tobias.anchorj.tabular.TabularInstance;
import de.viadee.anchorj.ClassificationFunction;
import hex.ModelCategory;
import hex.genmodel.ModelMojoReader;
import hex.genmodel.MojoModel;
import hex.genmodel.MojoReaderBackend;
import hex.genmodel.MojoReaderBackendFactory;
import hex.genmodel.easy.EasyPredictModelWrapper;
import hex.genmodel.easy.RowData;
import hex.genmodel.easy.exception.PredictException;
import hex.genmodel.easy.prediction.RegressionModelPrediction;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

// TODO Make abstract and have implementations for each model type. Let user define output discretization
public class H2OTabularRegressionMojoClassifier<T extends TabularInstance> implements ClassificationFunction<T> {

    private final EasyPredictModelWrapper modelWrapper;
    private final List<String> columnNames;

    public H2OTabularRegressionMojoClassifier(InputStream mojoInputStream) throws IOException {
        this(mojoInputStream, null);
    }

    public H2OTabularRegressionMojoClassifier(InputStream mojoInputStream, List<String> columnNames) throws IOException {
        final MojoReaderBackend reader = MojoReaderBackendFactory.createReaderBackend(mojoInputStream,
                MojoReaderBackendFactory.CachingStrategy.MEMORY);
        final MojoModel model = ModelMojoReader.readFrom(reader);
        this.modelWrapper = new EasyPredictModelWrapper(model);
        if (columnNames == null) {
            columnNames = Arrays.asList(model.getNames());
        }
        this.columnNames = Collections.unmodifiableList(columnNames);
    }

    @Override
    public int predict(T instance) {
        if (columnNames.size() != instance.getFeatureCount())
            throw new IllegalArgumentException("ColumnNames size does not match instance's feature count");

        RowData row = new RowData();
        int i = 0;
        for (String columnName : columnNames) {
            Object value = instance.getInstance()[i++];
            if (value instanceof Integer) {
                value = ((Integer) value).doubleValue();
            }
            row.put(columnName, value);
        }

        try {
            RegressionModelPrediction prediction = (RegressionModelPrediction) modelWrapper.predict(row, ModelCategory.Regression);
            return (int) prediction.value;
        } catch (PredictException e) {
            throw new RuntimeException(e);
        }
    }
}
