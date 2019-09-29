package dataInitialization.impl;

import dataInitialization.DataInitializer;
import configurationSpace.DiscretizerInstantiation.PercentileMedianDiscretizerInstantiation;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.column.StringColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Map;

public class TitanicInitializer implements DataInitializer {

    @Override
    public AnchorTabular createTabular(Map<String, Discretizer> discretizers) {
        InputStream data = ClassLoader.getSystemResourceAsStream("Titanic/titanic.csv");
        if (data == null)
            throw new RuntimeException("Could not load data");

        try {
            return new AnchorTabularBuilderByName()
                    .setDoBalance(false)
                    .addTargetColumn(IntegerColumn.fromStringInput("Survived"))
                    .addColumn(IntegerColumn.fromStringInput("Pclass"))
                    .addColumn(new StringColumn("Name"))
                    .addColumn(new StringColumn("Sex"))
                    .addColumn(DoubleColumn.fromStringInput("Age", -1, discretizers != null ? discretizers.get("Age") : new PercentileMedianDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("SibSp"))
                    .addColumn(IntegerColumn.fromStringInput("Parch"))
                    .addColumn(IntegerColumn.fromStringInput("SameTicket"))
                    .addColumn(DoubleColumn.fromStringInput("Fare", -1, discretizers != null ? discretizers.get("Fare") : new PercentileMedianDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(new StringColumn("Cabin"))
                    .addColumn(new StringColumn("Embarked"))
                    .build(data, false);
        } catch (IOException e) {
            e.printStackTrace();
            throw new RuntimeException("Could not read data");
        }
    }

    private static final class TicketNumberTransformer implements Transformer {

        @Override
        public Serializable apply(Serializable serializable) {
            // Transforms the ticket column to contain only the ticket number without ticket prefixes
            String replaced = ((String) serializable).replaceAll("[^\\d]+", "");

            return ("".equals(replaced)) ? -1 : replaced;
        }
    }
}
