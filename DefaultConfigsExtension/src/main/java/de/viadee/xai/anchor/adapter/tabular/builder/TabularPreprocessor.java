package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.adapter.tabular.TabularInstanceVisualizer;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;
import de.viadee.xai.anchor.adapter.tabular.util.ArrayUtils;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Performs all preprocessing steps that are defined by the columns to
 * ultimately come up with an {@link AnchorTabular} instance
 */
final class TabularPreprocessor {


    /**
     * Creates the {@link AnchorTabular} instance
     *
     * @param columns      all {@link GenericColumn}s, one for each column defined in data
     * @param targetColumn the targetColumn whose value is to be predicted. Also contained in columns.
     * @param data         the actual data, e.g., loaded from a *.CSV file
     * @return the {@link AnchorTabular} instance usable for further processing
     */
    static AnchorTabular createAnchorTabular(final Collection<GenericColumn> columns,
                                             final GenericColumn targetColumn,
                                             Collection<String[]> data,
                                             boolean doBalance) {

        final DataFrame dataFrame = new DataFrame(columns.toArray(new GenericColumn[0]), data);

        removeUnusedColumns(dataFrame);
        applyTransformations(dataFrame);
        fitDiscretizers(dataFrame);


        // Split off labels
        Serializable[] transformedLabels = null;
        Double[] discretizedLabels = null;
        if (targetColumn != null) {
            transformedLabels = dataFrame.removeColumn(targetColumn);
            discretizedLabels = targetColumn.getDiscretizer().apply(transformedLabels);
        }
        // Apply Discretization
        final Double[][] discretizedData = discretizeData(dataFrame);

        // Create TabularInstances
        TabularInstance[] instances = new TabularInstance[dataFrame.getRowCount()];
        final GenericColumn[] finalColumns = dataFrame.getColumns().toArray(new GenericColumn[0]);

        final DataFrame discretizedDataFrame = new DataFrame(finalColumns, discretizedData);

        for (int i = 0; i < instances.length; i++) {
            instances[i] = new TabularInstance(
                    finalColumns,
                    targetColumn,
                    dataFrame.getRow(i),
                    ArrayUtils.transformToDoubleArray(discretizedDataFrame.getRow(i)),
                    (transformedLabels != null) ? transformedLabels[i] : null,
                    (discretizedLabels != null) ? discretizedLabels[i] : null
            );
        }

        // Balance dataset if set
        if (doBalance) {
            instances = Balancer.balance(instances);
        }

        return new AnchorTabular(instances, new TabularInstanceVisualizer());
    }


    /**
     * Removes all columns that are described to be not usable
     *
     * @param dataFrame the {@link DataFrame} that is currently being used
     */
    private static void removeUnusedColumns(DataFrame dataFrame) {
        // First extract columns or else: ConcurrentModification
        List<String> columnsToDelete = dataFrame.getColumns().stream().filter(column1 -> !column1.isDoUse())
                .map(GenericColumn::getName).collect(Collectors.toList());
        for (String columnToDelete : columnsToDelete) {
            dataFrame.removeColumn(columnToDelete);
        }
    }


    /**
     * Applies all transformations directly to the dataFrame.
     *
     * @param dataFrame the dataframe to apply discretization to
     */
    private static void applyTransformations(DataFrame dataFrame) {
        // apply all transformations of every column
        for (GenericColumn column : dataFrame.getColumns()) {
            for (Transformer transformer : column.getTransformers()) {
                dataFrame.transformColumn(column, transformer);
            }
        }
    }

    /**
     * Fits all discretizers
     *
     * @param dataFrame the {@link DataFrame} that is currently being used
     */
    private static void fitDiscretizers(final DataFrame dataFrame) {
        // Fit discretizers set in columns. Do not transform yet
        for (GenericColumn column : dataFrame.getColumns()) {
            if (column.getDiscretizer() == null) {
                column.setDiscretizer(new UniqueValueDiscretizer());
            }
            column.getDiscretizer().fit(dataFrame.getColumn(column));
        }
    }

    /**
     * Applies all discretizers
     *
     * @param dataFrame the dataframe to apply discretization to
     * @return the discretized data
     */
    private static Double[][] discretizeData(DataFrame dataFrame) {
        Double[][] discretizedData = new Double[dataFrame.getColumnCount()][];
        for (int i = 0; i < dataFrame.getColumnCount(); i++) {
            GenericColumn column = dataFrame.getColumns().get(i);
            discretizedData[i] = dataFrame.discretizeColumn(column, column.getDiscretizer());
        }
        return discretizedData;
    }


    ///**
    // * Splits the dataset into three subsets, i.e. training, validation and test set
    // *
    // * @param firstSplit  the size of the first split in percent
    // * @param secondSplit size of the second split
    // * @return the result, having three main indices
    // */
    //public TabularInstance[][] shuffleSplitInstances(double firstSplit, double secondSplit) {
    //    TabularInstance[][] firstShuffleSplitResult = ShuffleSplit.shuffleSplit(this.tabularInstances, firstSplit);
    //    TabularInstance[][] secondShuffleSplitResult = ShuffleSplit.shuffleSplit(firstShuffleSplitResult[1], secondSplit);
    //    return new TabularInstance[][]{firstShuffleSplitResult[0], secondShuffleSplitResult[0], secondShuffleSplitResult[1]};
    //}


}
