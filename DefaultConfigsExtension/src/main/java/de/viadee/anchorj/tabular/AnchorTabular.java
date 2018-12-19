package de.viadee.anchorj.tabular;

import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

import de.viadee.anchorj.AnchorConstructionBuilder;
import de.viadee.anchorj.tabular.column.GenericColumn;
import de.viadee.anchorj.tabular.column.IgnoredColumn;
import de.viadee.anchorj.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.anchorj.tabular.transformations.PreProcessor;
import de.viadee.anchorj.tabular.util.Balancer;
import de.viadee.anchorj.tabular.util.CSVReader;
import de.viadee.anchorj.tabular.util.ShuffleSplit;

import static de.viadee.anchorj.tabular.util.ArrayUtils.extractColumn;
import static de.viadee.anchorj.tabular.util.ArrayUtils.removeColumn;

/**
 * Provides default means to use the Anchors algorithm on tabular data
 * <p>
 * To make use of this, use the {@link Builder} to create an instance of this class.
 */
public class AnchorTabular {

    private final GenericColumn[] columns;
    private final GenericColumn targetColumn;
    private final TabularInstanceVisualizer tabularInstanceVisualizer;
    private final LinkedList<PreProcessor> preProcessors;
    private TabularInstance[] tabularInstances;

    private AnchorTabular(final GenericColumn[] columns,
                          final GenericColumn targetColumn,
                          final LinkedList<PreProcessor> preProcessors,
                          final TabularInstanceVisualizer tabularInstanceVisualizer) {
        this.columns = columns;
        this.targetColumn = targetColumn;
        this.preProcessors = preProcessors;
        this.tabularInstanceVisualizer = tabularInstanceVisualizer;
    }

    public static TabularInstance[] preprocessData(AnchorTabular tabular,
                                                   final Collection<String[]> dataCollection,
                                                   boolean doBalance) {
        Serializable[][] transformedData = mapCollectionToArray(dataCollection);
        for (PreProcessor preprocessor : tabular.preProcessors) {
            transformedData = preprocessor.apply(transformedData);
        }

        applyTransformations(transformedData, tabular.columns);

        Integer[][] discretizedData = new Integer[transformedData.length][];

        BiFunction<Serializable[], GenericColumn, Integer[]> columnDiscretizer = (transformedColumn, column) -> {
            column.getDiscretizer().fit(transformedColumn);
            return column.getDiscretizer().apply(transformedColumn);
        };
        // Apply all discretizers
        for (int i = 0; i < tabular.columns.length; i++) {
            final Serializable[] transformedColumn = new Serializable[transformedData.length];
            for (int j = 0; j < transformedColumn.length; j++) {
                transformedColumn[j] = transformedData[j][i];
            }
            final GenericColumn usedColumn = tabular.columns[i];

            // Discretize. If no discretizer is set, set a default one
            if (usedColumn.getDiscretizer() == null) {
                usedColumn.setDiscretizer(new UniqueValueDiscretizer());
            }

            Integer[] discretizedColumn = columnDiscretizer.apply(transformedColumn, usedColumn);

            // Put discretized results into new array and create mapping to save efforts at runtime
            for (int j = 0; j < transformedColumn.length; j++) {
                if (i == 0) {
                    discretizedData[j] = new Integer[tabular.columns.length];
                }
                discretizedData[j][i] = discretizedColumn[j];
            }
        }

        // Split off labels
        Serializable[] transformedLabels = null;
        Integer[] discretizedLabels = null;
        if (tabular.targetColumn != null) {
            final int labelColumnIndex = tabular.targetColumn.getOriginalColumnIndex();
            transformedLabels = extractColumn(transformedData, labelColumnIndex);
            discretizedLabels = columnDiscretizer.apply(transformedLabels, tabular.targetColumn);
            transformedData = removeColumn(transformedData, labelColumnIndex);
        }

        TabularInstance[] instances = new TabularInstance[transformedData.length];
        for (int i = 0; i < transformedData.length; i++) {
            instances[i] = new TabularInstance(tabular.columns, tabular.targetColumn,
                    transformedData[i], discretizedData[i], (transformedLabels != null) ? transformedLabels[i] : null,
                    (discretizedLabels != null) ? discretizedLabels[i] : null);
        }

        // Balance dataset if set
        if (doBalance) {
            instances = Balancer.balance(instances);
        }

        return instances;
    }

    public TabularInstance[] getTabularInstances() {
        return tabularInstances;
    }

    protected void setTabularInstances(TabularInstance[] tabularInstances) {
        this.tabularInstances = tabularInstances;
    }

    private static AnchorTabular createAnchorTabular(final List<GenericColumn> columns,
                                                     final GenericColumn targetColumn) {
        LinkedList<PreProcessor> preProcessors = new LinkedList<>();
        removeUnusedColumns(preProcessors, columns);
        List<GenericColumn> usedColumns = columns.stream().filter(GenericColumn::isDoUse).collect(Collectors.toList());
        if (targetColumn != null) {
            usedColumns.remove(targetColumn);
        }

        // Create the result explainer
        TabularInstanceVisualizer tabularInstanceVisualizer = new TabularInstanceVisualizer();

        return new AnchorTabular(usedColumns.toArray(new GenericColumn[0]), targetColumn, preProcessors, tabularInstanceVisualizer);
    }

    /**
     * Iterates through the column description and removes all ignored columns.
     *
     * @param columnDescription list of columns
     */
    private static void removeUnusedColumns(LinkedList<PreProcessor> preProcessors,
                                            List<GenericColumn> columnDescription) {
        for (int i = 0; i < columnDescription.size(); i++) {
            if (!columnDescription.get(i).isDoUse()) {
                preProcessors.add(new PreProcessor(i) {
                    private static final long serialVersionUID = -6000642244021214574L;

                    @Override
                    public Serializable[][] apply(Serializable[][] serializables) {
                        return removeColumn(serializables, getProcessColumn());
                    }
                });
            }
        }
    }

    private static void applyTransformations(Serializable[][] data, GenericColumn[] internalColumns) {
        for (int i = 0; i < internalColumns.length; i++) {
            Serializable[] column = new Serializable[data.length];
            for (int j = 0; j < data.length; j++) {
                column[j] = data[j][i];
            }
            Serializable[] transformationResult;
            try {
                transformationResult = internalColumns[i].transform(column);
            } catch (Exception e) {
                throw new IllegalArgumentException("Could not transform column " + internalColumns[i] + " as specified.", e);
            }
            for (int j = 0; j < data.length; j++) {
                data[j][i] = transformationResult[j];
            }
        }
    }

    private static Serializable[][] removeColumns(Serializable[][] values, List<Integer> indices) {
        Serializable[][] result = new Serializable[values.length][];
        for (int i = 0; i < result.length; i++) {
            Serializable[] subResult = new Serializable[values[i].length - indices.size()];
            int currentIndex = 0;
            for (int j = 0; j < values[i].length; j++) {
                if (indices.contains(j))
                    continue;
                subResult[currentIndex++] = values[i][j];
            }
            result[i] = subResult;
        }
        return result;
    }

    private static Serializable[][] mapCollectionToArray(Collection<String[]> data) {
        if (data.size() < 1 || data.stream().mapToInt(d -> d.length).distinct().count() != 1)
            throw new RuntimeException("No data submitted or rows are differently sized");

        final int rowLength = data.iterator().next().length;
        Serializable[][] convertedData = new Serializable[data.size()][];
        Iterator<String[]> dataIter = data.iterator();
        for (int i = 0; i < data.size(); i++) {
            Serializable[] dataRow = new Serializable[rowLength];
            String[] oldRow = dataIter.next();
            System.arraycopy(oldRow, 0, dataRow, 0, rowLength);
            convertedData[i] = dataRow;
        }
        return convertedData;
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
        return new TabularInstance[][] { firstShuffleSplitResult[0], secondShuffleSplitResult[0], secondShuffleSplitResult[1] };
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
        return Collections.unmodifiableList(Arrays.asList(columns));
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
            if (targetColumn == null)
                throw new IllegalArgumentException("Not target column specified");
            for (String[] fileContent : dataCollection) {
                if (fileContent.length != columnDescriptions.size()) {
                    throw new IllegalArgumentException("InternalColumn count does not match loaded data's columns. " +
                            fileContent.length + " vs " + columnDescriptions.size());
                }
            }

            if (excludeFirst) {
                final Iterator<String[]> iterator = dataCollection.iterator();
                iterator.next();
                iterator.remove();
            }

            AnchorTabular tabular = AnchorTabular.createAnchorTabular(this.columnDescriptions, this.targetColumn);
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
            this.columnDescriptions.add(new IgnoredColumn(name, this.columnDescriptions.size()));
            return this;
        }

        /**
         * Marks a column to be ignored
         *
         * @param column a column for reasons of clarity and comprehensibility
         * @return the {@link Builder}
         */
        public Builder addIgnoredColumn(GenericColumn column) {
            this.columnDescriptions.add(new IgnoredColumn(column.getName(), column.getOriginalColumnIndex()));
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
