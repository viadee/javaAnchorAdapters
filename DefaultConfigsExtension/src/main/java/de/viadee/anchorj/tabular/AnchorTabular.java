package de.viadee.anchorj.tabular;

import de.viadee.anchorj.AnchorConstructionBuilder;
import de.viadee.anchorj.ClassificationFunction;
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
        // Add target column temporarily. Will be removed after data processed
        columns.add(targetColumn);
        // Read data to object
        Object[][] data = mapCollectionToArray(dataCollection);
        data = removeUnusedColumns(columns, data);
        List<AbstractColumn> usedColumns = columns.stream().filter(AbstractColumn::isDoUse).collect(Collectors.toList());

        // Apply transformation to used columns
        applyTransformations(data, usedColumns.toArray(new AbstractColumn[0]));

        // Store the mappings that were conducted in order to be able to reverse them later on
        final Map<AbstractColumn, Map<Object, Object>> mappings = new HashMap<>();

        // Apply all discretizers
        for (int i = 0; i < usedColumns.size(); i++) {
            final Object[] columnValues = new Object[data.length];
            for (int j = 0; j < columnValues.length; j++)
                columnValues[j] = data[j][i];
            final AbstractColumn usedColumn = usedColumns.get(i);
            if (usedColumn.getDiscretizer() == null)
                continue;
            usedColumn.getDiscretizer().fit(columnValues);
            for (int j = 0; j < columnValues.length; j++) {
                final Object originalValue = columnValues[j];
                final Object discretizedValue = usedColumn.getDiscretizer().apply(originalValue);
                data[j][i] = discretizedValue;
                mappings.computeIfAbsent(usedColumn, k -> new HashMap<>())
                        .putIfAbsent(originalValue, discretizedValue);
            }
        }


        // Split off labels
        int[] labels = null;
        if (!targetColumn.getDiscretizer().isResultNumeric())
            throw new IllegalArgumentException("Target Column must be discretized to int value");
        final int labelColumnIndex = usedColumns.indexOf(targetColumn);
        labels = Stream.of(extractIntegerColumn(data, labelColumnIndex)).mapToInt(i -> i).toArray();
        data = removeColumn(data, labelColumnIndex);

        // Finally remove target column
        usedColumns.remove(targetColumn);

        TabularInstanceList instances = new TabularInstanceList(data, labels, usedColumns);
        if (doBalance) {
            instances = instances.balance();
        }

        // Create the result explainer
        TabularInstanceVisualizer tabularInstanceVisualizer = new TabularInstanceVisualizer(mappings);

        return new AnchorTabular(instances, usedColumns.toArray(new AbstractColumn[0]), targetColumn, mappings, tabularInstanceVisualizer);
    }

//    private static Map<String, Integer> filterFeatureNamesByUsage(Map<String, Integer> featureNames, List<AbstractColumn> columnDescription, List<AbstractColumn> usedColumns) {
//        int newColumnIndex = 0;
//        Map<String, Integer> usedFeatureNames = new HashMap<>(usedColumns.size());
//
//        for (Map.Entry<String, Integer> entry : featureNames.entrySet().stream().sorted(Comparator.comparingInt(Map.Entry::getValue)).collect(Collectors.toList())) {
//            if (columnDescription.stream().anyMatch((column) -> column.getName().equals(entry.getKey()) && column.isDoUse())) {
//                usedFeatureNames.put(entry.getKey(), newColumnIndex);
//                newColumnIndex++;
//            }
//        }
//        return usedFeatureNames;
//    }

//    private static Map<AbstractColumn, Map<Object, FeatureValueMapping>> createTransformationMapping(Object[][] data, AbstractColumn[] usedColumns) {
//        Map<AbstractColumn, Map<Object, FeatureValueMapping>> mappings = new LinkedHashMap<>();
//
//        // Transform categorical columns to be in a range of 0..(distinct values)
//        // Also discretize nominal values
//        for (int i = 0; i < usedColumns.size(); i++) {
//            AbstractColumn internalColumn = usedColumns.get(i);
//
//            // Only categorize categorical columns
//            if (internalColumn.getColumnType() == ColumnType.CATEGORICAL) {
//                Object[] result = transformColumnToUniqueValues(data, i);
//                Map<Object, Object> transformedMapping = (Map<Object, Object>) result[1];
//
//                final AbstractColumn feature = finalFeatures[i];
//                Map<Object, FeatureValueMapping> featureMapping = new HashMap<>(transformedMapping.size());
//                transformedMapping.forEach((key, value) -> featureMapping.put(key, new CategoricalValueMapping(feature, key, value)));
//
//                mappings.put(feature, featureMapping);
//                replaceColumnValues(data, (int[]) result[0], i);
//            }
//
//            // Discretize nominal values if discretizer given
//            else if (internalColumn.getColumnType() == ColumnType.NOMINAL && internalColumn.getDiscretizer() != null) {
//                Number[] valuesToBeDiscretized = new Number[data.length];
//                for (int j = 0; j < valuesToBeDiscretized.length; j++) {
//                    if (data[j][i] instanceof String)
//                        valuesToBeDiscretized[j] = Double.valueOf((String) data[j][i]);
//                    else if (data[j][i] instanceof Integer)
//                        valuesToBeDiscretized[j] = (Integer) data[j][i];
//                    else
//                        throw new IllegalArgumentException("Could not read nominal column");
//                }
//                Object[] discretizedValues = internalColumn.getDiscretizer().apply(valuesToBeDiscretized);
//                // Discretized values come from a "range" of input values.
//                Map<Object, Set<Number>> tmpMapping = new LinkedHashMap<>();
//                for (int j = 0; j < discretizedValues.length; j++) {
//                    data[j][i] = discretizedValues[j];
//
//                    Set<Number> values;
//                    if (tmpMapping.containsKey(discretizedValues[j]))
//                        values = tmpMapping.get(discretizedValues[j]);
//                    else {
//                        values = new HashSet<>();
//                        tmpMapping.put(discretizedValues[j], values);
//                    }
//                    values.add(valuesToBeDiscretized[j]);
//                }
//
//                Map<Object, FeatureValueMapping> valueMappings = new LinkedHashMap<>();
//                for (Map.Entry<Object, Set<Number>> entry : tmpMapping.entrySet()) {
//                    Set<Double> values = entry.getValue().stream().map(Number::doubleValue).collect(Collectors.toSet());
//                    valueMappings.put(entry.getKey(), new MetricValueMapping(finalFeatures[i], entry.getKey(),
//                            Collections.min(values), Collections.max(values)));
//                }
//                mappings.put(finalFeatures[i], valueMappings);
//            } else {
//                mappings.put(finalFeatures[i], Collections.emptyMap());
//            }
//        }
//        return mappings;
//    }

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

//    /**
//     * Transforms an object column to an int column, where each class of unique objects has the same id
//     *
//     * @param values       the values
//     * @param targetColumn its column index
//     * @return an int array
//     */
//    private static Object[] transformColumnToUniqueValues(Object[][] values, int targetColumn) {
//        int[] result = new int[values.length];
//        Map<Object, Integer> valueSet = new LinkedHashMap<>();
//        for (int i = 0; i < values.length; i++) {
//            Object cell = values[i][targetColumn];
//            if (!valueSet.containsKey(cell)) {
//                valueSet.put(cell, valueSet.size());
//            }
//            result[i] = valueSet.get(cell);
//        }
//        return new Object[]{result, valueSet};
//    }

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
