package dataInitialization.impl;

import dataInitialization.DataInitializer;
import configurationSpace.DiscretizerInstantiation.RandomDiscretizerInstantiation;
import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.builder.AnchorTabularBuilderByName;
import de.viadee.xai.anchor.adapter.tabular.column.DoubleColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IntegerColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;

public class CpuInitializer implements DataInitializer {

    @Override
    public AnchorTabular createTabular(Map<String, Discretizer> discretizers) {
        InputStream trainingDataStream = ClassLoader.getSystemResourceAsStream("cpu_small/cpu_small_subset.csv");
        if (trainingDataStream == null)
            throw new RuntimeException("Could not load data");
        try {
            return new AnchorTabularBuilderByName()
                    .setDoBalance(false)
                    .addColumn(IntegerColumn.fromStringInput("lread", discretizers != null ?
                            discretizers.get("lread") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("lwrite", discretizers != null ?
                            discretizers.get("lwrite") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("scall", discretizers != null ?
                            discretizers.get("scall") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("sread", discretizers != null ?
                            discretizers.get("sread") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("swrite", discretizers != null ?
                            discretizers.get("swrite") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(DoubleColumn.fromStringInput("fork", -1,
                            discretizers != null ? discretizers.get("fork") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(DoubleColumn.fromStringInput("exec", -1,
                            discretizers != null ? discretizers.get("exec") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("rchar", discretizers != null ?
                            discretizers.get("rchar") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("wchar", discretizers != null ?
                            discretizers.get("wchar") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(DoubleColumn.fromStringInput("runqsz", -1,
                            discretizers != null ? discretizers.get("runqsz") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("freemem", discretizers != null ?
                            discretizers.get("freemem") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addColumn(IntegerColumn.fromStringInput("freeswap", discretizers != null ?
                            discretizers.get("freeswap") : new RandomDiscretizerInstantiation().getDefaultDiscretizer()))
                    .addTargetColumn(IntegerColumn.fromStringInput("usr"))
                    .build(trainingDataStream);
        } catch (IOException e) {
            throw new RuntimeException("Could not read data");
        }
    }
}
