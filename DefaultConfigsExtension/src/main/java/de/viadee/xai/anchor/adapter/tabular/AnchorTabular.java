package de.viadee.xai.anchor.adapter.tabular;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;
import de.viadee.xai.anchor.adapter.tabular.util.ArrayUtils;
import de.viadee.xai.anchor.adapter.tabular.util.Balancer;
import de.viadee.xai.anchor.adapter.tabular.util.ShuffleSplit;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Provides default means to use the Anchors algorithm on tabular data
 * <p>
 * To make use of this, use the AbstractTabularBuilder and its
 * various implementations to create an instance of this class.
 * <p>
 * TODO clean this up. Mostly unmaintainable code. Move static methods out. Make immutable
 */
public class AnchorTabular {
    private final GenericColumn targetColumn;
    private GenericColumn[] finalColumns;
    private final GenericColumn[] originalColumns;
    private final TabularInstanceVisualizer tabularInstanceVisualizer;
    private TabularInstance[] tabularInstances;

    private AnchorTabular(final GenericColumn[] originalColumns,
                          final GenericColumn targetColumn,
                          final TabularInstanceVisualizer tabularInstanceVisualizer) {
        this.originalColumns = originalColumns;
        this.targetColumn = targetColumn;
        this.tabularInstanceVisualizer = tabularInstanceVisualizer;
    }

    private static void fitData(final GenericColumn[] originalColumns, final Collection<String[]> dataCollection) {
        DataFrame dataFrame = new DataFrame(originalColumns, dataCollection);
        removeIgnoredColumns(dataFrame);
        applyTransformationsOfData(dataFrame);

        for (int i = 0; i < dataFrame.getNCols(); i++) {
            GenericColumn column = dataFrame.getColumns().get(i);
            // Discretize. If no discretizer is set, set a default one
            if (column.getDiscretizer() == null) {
                column.setDiscretizer(new UniqueValueDiscretizer());
            }

            column.getDiscretizer().fit(column.transformForAnchor(dataFrame.getColumn(column)));
        }
    }

    public static TabularInstance[] preprocessData(AnchorTabular tabular,
                                                   final Collection<String[]> dataCollection,
                                                   boolean doBalance) {

        DataFrame dataFrame = new DataFrame(tabular.originalColumns, dataCollection);
        removeIgnoredColumns(dataFrame);
        applyTransformationsOfData(dataFrame);

        // Split off labels
        Serializable[] transformedLabels = null;
        Integer[] discretizedLabels = null;
        if (tabular.targetColumn != null) {
            transformedLabels = dataFrame.removeColumn(tabular.targetColumn);
            transformedLabels = tabular.targetColumn.transformForAnchor(transformedLabels);
            discretizedLabels = tabular.targetColumn.getDiscretizer().apply(transformedLabels);
        }

        Serializable[][] anchorTransformedData = applyTransformationsForAnchor(dataFrame);
        Integer[][] discretizedData = discretizeData(new DataFrame(dataFrame.getColumns().toArray(new GenericColumn[0]),
                anchorTransformedData));

        TabularInstance[] instances = convertToTabularInstances(tabular, dataFrame, transformedLabels,
                discretizedLabels, discretizedData);

        // Balance dataset if set
        if (doBalance) {
            instances = Balancer.balance(instances);
        }

        return instances;
    }

    private static void removeIgnoredColumns(DataFrame dataFrame) {
        List<String> columnsToDelete = dataFrame.getColumns().stream().filter(column1 -> !column1.isDoUse())
                .map(GenericColumn::getName).collect(Collectors.toList());
        for (String columnToDelete : columnsToDelete) {
            dataFrame.removeColumn(columnToDelete);
        }
    }

    private static TabularInstance[] convertToTabularInstances(AnchorTabular tabular, DataFrame dataFrame,
                                                               Serializable[] transformedLabels,
                                                               Integer[] discretizedLabels,
                                                               Integer[][] discretizedData) {
        TabularInstance[] instances = new TabularInstance[dataFrame.getNRows()];
        final GenericColumn[] columns = dataFrame.getColumns().toArray(new GenericColumn[0]);
        tabular.finalColumns = columns;

        DataFrame discretizedDataFrame = new DataFrame(columns, discretizedData);
        for (int i = 0; i < instances.length; i++) {
            instances[i] = new TabularInstance(
                    columns,
                    tabular.targetColumn,
                    dataFrame.getRow(i),
                    ArrayUtils.transformToIntArray(discretizedDataFrame.getRow(i)),
                    (transformedLabels != null) ? transformedLabels[i] : null,
                    (discretizedLabels != null) ? discretizedLabels[i] : null
            );
        }
        return instances;
    }

    private static Integer[][] discretizeData(DataFrame dataFrame) {
        Integer[][] discretizedData = new Integer[dataFrame.getNCols()][];
        for (int i = 0; i < dataFrame.getNCols(); i++) {
            GenericColumn column = dataFrame.getColumns().get(i);
            discretizedData[i] = dataFrame.discretizeColumn(column, column.getDiscretizer());
        }
        return discretizedData;
    }

    private static void applyTransformationsOfData(DataFrame dataFrame) {
        // apply all transformations of every column
        for (GenericColumn column : dataFrame.getColumns()) {
            for (Transformer transformer : column.getDataTransformations()) {
                dataFrame.transformColumn(column, transformer);
            }
        }
    }

    private static Serializable[][] applyTransformationsForAnchor(DataFrame dataFrame) {
        // apply all transformations of every column
        Serializable[][] anchorTransformedData = new Serializable[dataFrame.getNCols()][];
        for (GenericColumn column : dataFrame.getColumns()) {
            anchorTransformedData[dataFrame.getColumnIndex(column)] = column.transformForAnchor(dataFrame.getColumn(column));
        }

        return anchorTransformedData;
    }

    public TabularInstance[] getTabularInstances() {
        return tabularInstances;
    }

    public void setTabularInstances(TabularInstance[] tabularInstances) {
        this.tabularInstances = tabularInstances;
    }

    public static AnchorTabular createAnchorTabular(final Collection<GenericColumn> columns,
                                                    final GenericColumn targetColumn,
                                                    Collection<String[]> data) {

        GenericColumn[] columnsArray = columns.toArray(new GenericColumn[0]);
        // Create the result explainer
        TabularInstanceVisualizer tabularInstanceVisualizer = new TabularInstanceVisualizer();
        fitData(columnsArray, data);

        return new AnchorTabular(
                columnsArray,
                targetColumn,
                tabularInstanceVisualizer
        );
    }


    /**
     * Splits the dataset into three subsets, i.e. training, validation and test set
     *
     * @param firstSplit  the size of the first split in percent
     * @param secondSplit size of the second split
     * @return the result, having three main indices
     */
    public TabularInstance[][] shuffleSplitInstances(double firstSplit, double secondSplit) {
        TabularInstance[][] firstShuffleSplitResult = ShuffleSplit.shuffleSplit(this.tabularInstances, firstSplit);
        TabularInstance[][] secondShuffleSplitResult = ShuffleSplit.shuffleSplit(firstShuffleSplitResult[1], secondSplit);
        return new TabularInstance[][]{firstShuffleSplitResult[0], secondShuffleSplitResult[0], secondShuffleSplitResult[1]};
    }

    /**
     * Provides a default builder configures with the contents of this class
     *
     * @param classificationFunction the classificationFunction to use
     * @param explainedInstance      the instance to explain
     * @return the further configurable or directly usable builder
     */
    public AnchorConstructionBuilder<TabularInstance> createDefaultBuilder(final Function<TabularInstance, Integer> classificationFunction,
                                                                           final TabularInstance explainedInstance) {
        TabularPerturbationFunction tabularPerturbationFunction = new TabularPerturbationFunction(explainedInstance,
                this.tabularInstances);
        return new AnchorConstructionBuilder<>(classificationFunction::apply, tabularPerturbationFunction, explainedInstance);
    }

    /**
     * @return an UnmodifiableList of the contained columns
     */
    public List<GenericColumn> getColumns() {
        return Arrays.asList(finalColumns);
    }

    /**
     * @return an instance of the {@link TabularInstanceVisualizer} to visualize explanations and instances
     */
    public TabularInstanceVisualizer getVisualizer() {
        return tabularInstanceVisualizer;
    }

    public GenericColumn getTargetColumn() {
        return targetColumn;
    }
}
