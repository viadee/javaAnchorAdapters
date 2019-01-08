package de.viadee.anchorj.tabular;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import de.viadee.anchorj.AnchorConstructionBuilder;
import de.viadee.anchorj.tabular.column.GenericColumn;
import de.viadee.anchorj.tabular.column.IgnoredColumn;
import de.viadee.anchorj.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.anchorj.tabular.transformations.Transformer;
import de.viadee.anchorj.tabular.util.ArrayUtils;
import de.viadee.anchorj.tabular.util.Balancer;
import de.viadee.anchorj.tabular.util.CSVReader;
import de.viadee.anchorj.tabular.util.ShuffleSplit;

/**
 * Provides default means to use the Anchors algorithm on tabular data
 * <p>
 * To make use of this, use the {@link Builder} to create an instance of this class.
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

    private static TabularInstance[] convertToTabularInstances(AnchorTabular tabular, DataFrame dataFrame, Serializable[] transformedLabels, Integer[] discretizedLabels, Integer[][] discretizedData) {
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

    protected void setTabularInstances(TabularInstance[] tabularInstances) {
        this.tabularInstances = tabularInstances;
    }

    private static AnchorTabular createAnchorTabular(final List<GenericColumn> columns,
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

    /**
     * Used to construct an {@link AnchorTabular} instance.
     * <p>
     * The addColumn operations must be called as many times as there are columns in the submitted dataset.
     */
    public static class Builder {
        private final List<GenericColumn> columnDescriptions = new ArrayList<>();
        private GenericColumn targetColumn;
        private boolean doBalance = false;

        /**
         * Constructs the builder
         */
        public Builder() {
        }

        /**
         * Builds the configured instance using a CSV file
         *
         * @param csvInputStream the inputStream
         * @return the {@link AnchorTabular} instance
         * @throws IOException when the CSV cannot be parsed
         */
        public AnchorTabular build(InputStream csvInputStream) throws IOException {
            return build(csvInputStream, false, false);
        }

        /**
         * Builds the configured instance using a CSV file
         *
         * @param csvInputStream the inputStream
         * @param excludeFirst   exclude the first row. Helpful if it is the header row
         * @param trim           if true, each on each cell String#trim will be called
         * @return the {@link AnchorTabular} instance
         * @throws IOException when the CSV cannot be parsed
         */
        public AnchorTabular build(InputStream csvInputStream, boolean excludeFirst, boolean trim) throws IOException {
            Collection<String[]> strings = CSVReader.readCSV(csvInputStream, trim);
            return build(strings, excludeFirst);
        }

        /**
         * Builds the configured instance
         *
         * @param dataCollection the date to be transformed
         * @return the {@link AnchorTabular} instance
         */
        public AnchorTabular build(Collection<String[]> dataCollection) {
            return this.build(dataCollection, false);
        }

        /**
         * Builds the configured instance
         *
         * @param dataCollection the data to be transformed
         * @param excludeFirst   exclude the first row. Helpful if it is the header row
         * @return the {@link AnchorTabular} instance
         */
        public AnchorTabular build(Collection<String[]> dataCollection, boolean excludeFirst) {
            if (targetColumn == null) {
                throw new IllegalArgumentException("Not target column specified");
            }
            if (dataCollection.size() <= 0) {
                throw new IllegalArgumentException("No data passed");
            }
            final int columnLength = columnDescriptions.size();
            if (!dataCollection.stream().parallel().allMatch((dataRow) -> dataRow.length == columnLength)) {
                throw new IllegalArgumentException("InternalColumn count does not match loaded data's columns");
            }

            if (excludeFirst) {
                final Iterator<String[]> iterator = dataCollection.iterator();
                iterator.next();
                iterator.remove();
            }

            AnchorTabular tabular = AnchorTabular.createAnchorTabular(this.columnDescriptions, this.targetColumn,
                    dataCollection);
            tabular.setTabularInstances(preprocessData(tabular, dataCollection, this.doBalance));

            return tabular;
        }


        /**
         * Registers a column
         *
         * @param column the column to be added
         * @return the {@link AnchorTabular} instance
         */
        public Builder addColumn(GenericColumn column) {
            this.columnDescriptions.add(column);
            return this;
        }

        /**
         * Marks a column to be ignored
         *
         * @return the {@link Builder}
         */
        public Builder addIgnoredColumn() {
            return addIgnoredColumn((String) null);
        }

        /**
         * Marks a column to be ignored
         *
         * @param name a name for reasons of clarity and comprehensibility
         * @return the {@link Builder}
         */
        public Builder addIgnoredColumn(String name) {
            this.columnDescriptions.add(new IgnoredColumn(name));
            return this;
        }

        /**
         * Marks a column to be ignored
         *
         * @param column a column for reasons of clarity and comprehensibility
         * @return the {@link Builder}
         */
        public Builder addIgnoredColumn(GenericColumn column) {
            this.columnDescriptions.add(new IgnoredColumn(column.getName()));
            return this;
        }

        /**
         * Registers a target column
         *
         * @param column the target column to be set
         * @return the {@link AnchorTabular} instance
         */
        public Builder addTargetColumn(GenericColumn column) {
            if (targetColumn != null)
                throw new IllegalArgumentException("Only one target column can be set");
            this.targetColumn = column;
            this.columnDescriptions.add(column);
            return this;
        }

        /**
         * May be used to configure the preprocessor to balance the dataset, i.e. to truncate the set of instances so
         * that each label has the same amount of instances
         *
         * @param doBalance true, if to balance dataset
         * @return the current {@link Builder}'s instance
         */
        public Builder setDoBalance(boolean doBalance) {
            this.doBalance = doBalance;
            return this;
        }
    }

}
