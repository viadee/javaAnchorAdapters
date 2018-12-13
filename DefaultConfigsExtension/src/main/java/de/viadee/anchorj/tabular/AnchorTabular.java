package de.viadee.anchorj.tabular;

import de.viadee.anchorj.AnchorConstructionBuilder;
import de.viadee.anchorj.tabular.column.GenericColumn;
import de.viadee.anchorj.tabular.column.IgnoredColumn;
import de.viadee.anchorj.tabular.discretizer.UniqueValueDiscretizer;
import de.viadee.anchorj.tabular.util.Balancer;
import de.viadee.anchorj.tabular.util.CSVReader;
import de.viadee.anchorj.tabular.util.ShuffleSplit;

import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static de.viadee.anchorj.util.ArrayUtils.*;

/**
 * Provides default means to use the Anchors algorithm on tabular data
 * <p>
 * To make use of this, use the {@link Builder} to create an instance of this class.
 */
public class AnchorTabular {

    private final TabularInstance[] tabularInstances;
    private final GenericColumn[] columns;
    private final GenericColumn targetColumn;
    private final Map<GenericColumn, Map<Object, Integer>> mappings;
    private final TabularInstanceVisualizer tabularInstanceVisualizer;

    private AnchorTabular(final TabularInstance[] tabularInstances, final GenericColumn[] columns,
                          final GenericColumn targetColumn,
                          final Map<GenericColumn, Map<Object, Integer>> mappings,
                          final TabularInstanceVisualizer tabularInstanceVisualizer) {
        this.tabularInstances = tabularInstances;
        this.columns = columns;
        this.targetColumn = targetColumn;
        this.mappings = mappings;
        this.tabularInstanceVisualizer = tabularInstanceVisualizer;
    }

    @SuppressWarnings("unchecked")
    private static AnchorTabular preprocess(final Collection<String[]> dataCollection,
                                            final List<GenericColumn> columns,
                                            final GenericColumn targetColumn,
                                            final boolean doBalance) {
        // Add target column temporarily. Will be removed after transformedData processed
        // columns.add(targetColumn);
        // Read transformedData to object
        Object[][] transformedData = removeUnusedColumns(columns, mapCollectionToArray(dataCollection));
        Integer[][] discretizedData = new Integer[transformedData.length][];
        List<GenericColumn> usedColumns = columns.stream().filter(GenericColumn::isDoUse).collect(Collectors.toList());

        // Apply transformation to used columns
        applyTransformations(transformedData, usedColumns.toArray(new GenericColumn[0]));

        // Store the mappings that were conducted in order to be able to reverse them later on
        final Map<GenericColumn, Map<Object, Integer>> mappings = new HashMap<>();

        // Apply all discretizers
        for (int i = 0; i < usedColumns.size(); i++) {
            final Object[] originalColumn = new Object[transformedData.length];
            for (int j = 0; j < originalColumn.length; j++)
                originalColumn[j] = transformedData[j][i];
            final GenericColumn usedColumn = usedColumns.get(i);

            // Discretize. If no discretizer is set, set a default one
            if (usedColumn.getDiscretizer() == null) {
                // Use Array copy to ensure later manipulations don't affect both data
                //System.arraycopy(originalColumn, 0, discretizedColumn, 0, originalColumn.length);
                usedColumn.setDiscretizer(new UniqueValueDiscretizer());
            }

            usedColumn.getDiscretizer().fit(originalColumn);
            final Integer[] discretizedColumn = new Integer[transformedData.length];
            for (int j = 0; j < originalColumn.length; j++) {
                final Object originalValue = originalColumn[j];
                discretizedColumn[j] = usedColumn.getDiscretizer().apply(originalValue);
            }

            // Put discretized results into new array and create mapping to save efforts at runtime
            for (int j = 0; j < originalColumn.length; j++) {
                if (i == 0)
                    discretizedData[j] = new Integer[usedColumns.size()];
                discretizedData[j][i] = discretizedColumn[j];
                mappings.computeIfAbsent(usedColumn, k -> new HashMap<>())
                        .putIfAbsent(transformedData[j][i], discretizedColumn[j]);
            }
        }

        // Split off labels
        Object[] transformedLabels = null;
        int[] discretizedLabels = null;
        if (targetColumn != null) {
            final int labelColumnIndex = usedColumns.indexOf(targetColumn);
            transformedLabels = extractObjectColumn(transformedData, labelColumnIndex);
            discretizedLabels = Stream.of(extractIntegerColumn(discretizedData, labelColumnIndex)).mapToInt(i -> i).toArray();
            transformedData = removeColumn(transformedData, labelColumnIndex);
            discretizedData = removeIntegerColumn(discretizedData, labelColumnIndex);

            // Finally remove target column
            usedColumns.remove(targetColumn);
        }

        TabularInstance[] instances = new TabularInstance[transformedData.length];
        for (int i = 0; i < transformedData.length; i++) {
            instances[i] = new TabularInstance(usedColumns.toArray(new GenericColumn[0]), targetColumn,
                    transformedData[i], discretizedData[i], (transformedLabels != null) ? transformedLabels[i] : null,
                    (discretizedLabels != null) ? discretizedLabels[i] : null);
        }

        // Balance dataset if set
        if (doBalance) {
            instances = Balancer.balance(instances);
        }

        // Create the result explainer
        TabularInstanceVisualizer tabularInstanceVisualizer = new TabularInstanceVisualizer(mappings);

        return new AnchorTabular(instances, usedColumns.toArray(new GenericColumn[0]), targetColumn, mappings, tabularInstanceVisualizer);
    }

    /**
     * Iterates through the column description and removes all ignored columns.
     *
     * @param columnDescription list of columns
     * @param data              the 2D data array
     * @return the data without ignored columns
     */
    private static Object[][] removeUnusedColumns(List<GenericColumn> columnDescription, Object[][] data) {
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


    private static void applyTransformations(Object[][] data, GenericColumn[] internalColumns) {
        for (int i = 0; i < internalColumns.length; i++) {
            Object[] column = new Object[data.length];
            for (int j = 0; j < data.length; j++) {
                column[j] = data[j][i];
            }
            Object[] transformationResult;
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
     * Splits the dataset into three subsets, i.e. training, validation and test set
     *
     * @param firstSplit  the size of the first split in percent
     * @param secondSplit size of the second split
     * @return the result, having three main indices
     */
    public TabularInstance[][] shuffleSplitInstances(double firstSplit, double secondSplit) {
        TabularInstance[][] firstShuffleSplitResult = ShuffleSplit.shuffleSplit(tabularInstances, firstSplit);
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
                tabularInstances);
        return new AnchorConstructionBuilder<>(classificationFunction::apply, tabularPerturbationFunction, explainedInstance);
    }

    /**
     * @return the tabular instances created by transforming and discretizing the source
     */
    public TabularInstance[] getTabularInstances() {
        return tabularInstances;
    }

    /**
     * @return an UnmodifiableList of the contained columns
     */
    public List<GenericColumn> getColumns() {
        return Collections.unmodifiableList(Arrays.asList(columns));
    }

    /**
     * @return a {@link Map} mapping, for each feature, which value got replaced by which other value during
     * preprocessing
     */
    public Map<GenericColumn, Map<Object, Integer>> getMappings() {
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

            return AnchorTabular.preprocess(dataCollection, this.columnDescriptions, this.targetColumn, this.doBalance);
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
        public Builder addTargetcolumn(GenericColumn column) {
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
