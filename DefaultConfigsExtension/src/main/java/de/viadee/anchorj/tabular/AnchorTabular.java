package de.viadee.anchorj.tabular;

import de.viadee.anchorj.AnchorConstructionBuilder;
import de.viadee.anchorj.tabular.column.AbstractColumn;
import de.viadee.anchorj.tabular.column.IgnoredColumn;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.viadee.anchorj.util.ArrayUtils.extractIntegerColumn;
import static de.viadee.anchorj.util.ArrayUtils.removeColumn;

/**
 * Provides default means to use the Anchors algorithm on tabular data
 * <p>
 * To make use of this, use the {@link Builder} to create an instance of this class.
 */
public class AnchorTabular {

    private final TabularInstanceList tabularInstances;
    private final AbstractColumn[] columns;
    private final AbstractColumn targetColumn;
    private final Map<AbstractColumn, Map<Object, Object>> mappings;
    private final TabularInstanceVisualizer tabularInstanceVisualizer;

    private AnchorTabular(final TabularInstanceList tabularInstances, final AbstractColumn[] columns,
                          final AbstractColumn targetColumn,
                          final Map<AbstractColumn, Map<Object, Object>> mappings,
                          final TabularInstanceVisualizer tabularInstanceVisualizer) {
        this.tabularInstances = tabularInstances;
        this.columns = columns;
        this.targetColumn = targetColumn;
        this.mappings = mappings;
        this.tabularInstanceVisualizer = tabularInstanceVisualizer;
    }

    @SuppressWarnings("unchecked")
    private static AnchorTabular preprocess(final Collection<String[]> dataCollection,
                                            final List<AbstractColumn> columns,
                                            final AbstractColumn targetColumn,
                                            final boolean doBalance) {
        // Add target column temporarily. Will be removed after transformedData processed
        columns.add(targetColumn);
        // Read transformedData to object
        Object[][] transformedData = removeUnusedColumns(columns, mapCollectionToArray(dataCollection));
        Object[][] discretizedData = new Object[transformedData.length][];
        List<AbstractColumn> usedColumns = columns.stream().filter(AbstractColumn::isDoUse).collect(Collectors.toList());

        // Apply transformation to used columns
        applyTransformations(transformedData, usedColumns.toArray(new AbstractColumn[0]));

        // Store the mappings that were conducted in order to be able to reverse them later on
        final Map<AbstractColumn, Map<Object, Object>> mappings = new HashMap<>();

        // Apply all discretizers
        for (int i = 0; i < usedColumns.size(); i++) {
            final Object[] originalColumn = new Object[transformedData.length];
            for (int j = 0; j < originalColumn.length; j++)
                originalColumn[j] = transformedData[j][i];
            final AbstractColumn usedColumn = usedColumns.get(i);

            // Discretize or accept original values
            Object[] discretizedColumn = new Object[transformedData.length];
            if (usedColumn.getDiscretizer() == null) {
                // Use Array copy to ensure later manipulations don't affect both data
                System.arraycopy(originalColumn,  0, discretizedColumn, 0, originalColumn.length);
            } else {
                usedColumn.getDiscretizer().fit(originalColumn);
                for (int j = 0; j < originalColumn.length; j++) {
                    final Object originalValue = originalColumn[j];
                    discretizedColumn[j] = usedColumn.getDiscretizer().apply(originalValue);
                }
            }
            // Put discretized results into new array and create mapping to save efforts at runtime
            for (int j = 0; j < originalColumn.length; j++) {
                if (i == 0)
                    discretizedData[j] = new Object[usedColumns.size()];
                discretizedData[j][i] = discretizedColumn[j];
                mappings.computeIfAbsent(usedColumn, k -> new HashMap<>())
                        .putIfAbsent(discretizedColumn[j], transformedData[j][i]);
            }
        }


        // Split off labels
        int[] labels = null;
        if (!targetColumn.getDiscretizer().isResultNumeric())
            throw new IllegalArgumentException("Target Column must be discretized to int value");
        final int labelColumnIndex = usedColumns.indexOf(targetColumn);
        labels = Stream.of(extractIntegerColumn(discretizedData, labelColumnIndex)).mapToInt(i -> i).toArray();
        transformedData = removeColumn(transformedData, labelColumnIndex);
        discretizedData = removeColumn(discretizedData, labelColumnIndex);

        // Finally remove target column
        usedColumns.remove(targetColumn);

        TabularInstanceList instances = new TabularInstanceList(transformedData, labels, usedColumns);
        if (doBalance) {
            instances = instances.balance();
        }

        // Create the result explainer
        TabularInstanceVisualizer tabularInstanceVisualizer = new TabularInstanceVisualizer(mappings);

        return new AnchorTabular(instances, usedColumns.toArray(new AbstractColumn[0]), targetColumn, mappings, tabularInstanceVisualizer);
    }

    /**
     * Iterates through the column description and removes all ignored columns.
     *
     * @param columnDescription list of columns
     * @param data              the 2D data array
     * @return the data without ignored columns
     */
    private static Object[][] removeUnusedColumns(List<AbstractColumn> columnDescription, Object[][] data) {
        List<Integer> unusedIndices = new ArrayList<>();
        for (int i = 0; i < columnDescription.size(); i++) {
            if (!columnDescription.get(i).isDoUse())
                unusedIndices.add(i);
        }
        // Remove unused columns
        if (!unusedIndices.isEmpty()) {
            data = removeColumns(data, unusedIndices);
        }
        return data;
    }


    private static void applyTransformations(Object[][] data, AbstractColumn[] internalColumns) {
        for (int i = 0; i < internalColumns.length; i++) {
            Object[] column = new Object[data.length];
            for (int j = 0; j < data.length; j++) {
                column[j] = data[j][i];
            }
            Object[] transformationResult = internalColumns[i].transform(column);
            for (int j = 0; j < data.length; j++) {
                data[j][i] = transformationResult[j];
            }
        }
    }

    private static Object[][] removeColumns(Object[][] values, List<Integer> indices) {
        Object[][] result = new Object[values.length][];
        for (int i = 0; i < result.length; i++) {
            Object[] subResult = new Object[values[i].length - indices.size()];
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

    private static Object[][] mapCollectionToArray(Collection<String[]> data) {
        if (data.size() < 1 || data.stream().mapToInt(d -> d.length).distinct().count() != 1)
            throw new RuntimeException("No data submitted or rows are differently sized");

        final int rowLength = data.iterator().next().length;
        Object[][] convertedData = new Object[data.size()][];
        Iterator<String[]> dataIter = data.iterator();
        for (int i = 0; i < data.size(); i++) {
            Object[] dataRow = new Object[rowLength];
            String[] oldRow = dataIter.next();
            System.arraycopy(oldRow, 0, dataRow, 0, rowLength);
            convertedData[i] = dataRow;
        }
        return convertedData;
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
                tabularInstances.getInstances().toArray(new TabularInstance[0]));
        return new AnchorConstructionBuilder<>(classificationFunction::apply, tabularPerturbationFunction, explainedInstance);
    }

    /**
     * @return the contained instance list
     */
    public TabularInstanceList getTabularInstances() {
        return tabularInstances;
    }

    /**
     * @return an UnmodifiableList of the contained columns
     */
    public List<AbstractColumn> getColumns() {
        return Collections.unmodifiableList(Arrays.asList(columns));
    }

    /**
     * @return a {@link Map} mapping, for each feature, which value got replaced by which other value during
     * preprocessing
     */
    public Map<AbstractColumn, Map<Object, Object>> getMappings() {
        return mappings;
    }

    /**
     * @return an instance of the {@link TabularInstanceVisualizer} to visualize explanations and instances
     */
    public TabularInstanceVisualizer getVisualizer() {
        return tabularInstanceVisualizer;
    }

    /**
     * Used to construct an {@link AnchorTabular} instance.
     * <p>
     * The addColumn operations must be called as many times as there are columns in the submitted dataset.
     */
    public static class Builder {
        private final List<AbstractColumn> columnDescriptions = new ArrayList<>();
        private AbstractColumn targetColumn;
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
                // +1 == target feature
                if (fileContent.length != columnDescriptions.size() + 1) {
                    throw new IllegalArgumentException("InternalColumn count does not match loaded data's columns. " +
                            fileContent.length + " vs " + columnDescriptions.size());
                }
            }

            if (excludeFirst) {
                final Iterator<String[]> iterator = dataCollection.iterator();
                iterator.next();
                iterator.remove();
            }

            return AnchorTabular.preprocess(dataCollection, this.columnDescriptions, this.targetColumn, this.doBalance);
        }


        /**
         * Registers a column
         *
         * @param column the column to be added
         * @return the {@link AnchorTabular} instance
         */
        public Builder addColumn(AbstractColumn column) {
            this.columnDescriptions.add(column);
            return this;
        }

        public Builder addIgnoredColumn() {
            this.columnDescriptions.add(new IgnoredColumn());
            return this;
        }

        /**
         * Registers a target column
         *
         * @param column the target column to be set
         * @return the {@link AnchorTabular} instance
         */
        public Builder addTargetcolumn(AbstractColumn column) {
            if (targetColumn != null)
                throw new IllegalArgumentException("Only one target column can be set");
            this.targetColumn = column;
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
