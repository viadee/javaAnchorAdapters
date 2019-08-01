package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.util.CSVReader;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Used to construct an {@link AnchorTabular} instance.
 * <p>
 * Assigns a {@link GenericColumn} to every column that exists in the dataset.
 */
abstract class AbstractTabularBuilder<T extends AbstractTabularBuilder<T>> {
    private GenericColumn targetColumn;

    private boolean doBalance = false;

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
     * @return the {@link AnchorTabular} instance
     * @throws IOException when the CSV cannot be parsed
     */
    public AnchorTabular build(InputStream csvInputStream, boolean excludeFirst) throws IOException {
        return build(csvInputStream, excludeFirst, false);
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
        if ((!excludeFirst && dataCollection.size() <= 0) || (excludeFirst && dataCollection.size() <= 1)) {
            throw new IllegalArgumentException("No data passed");
        }

        if (dataCollection.stream().mapToInt(s -> s.length).distinct().count() != 1) {
            throw new IllegalArgumentException("Not all rows have the same number of columns");
        }

        if (excludeFirst) {
            final Iterator<String[]> iterator = dataCollection.iterator();
            String[] removedRow = iterator.next();
            iterator.remove();
            registerRemovedRow(removedRow);
        }

        final int columnSize = dataCollection.iterator().next().length;
        final Collection<GenericColumn> columnDescriptions = getColumnDescriptions(columnSize);

        if (columnDescriptions.size() != columnSize) {
            throw new IllegalArgumentException("Exactly one description needs to be defined for each columm");
        }

        // Pass the column values to the builder, if a listener has been set
        // This helps to e.g. let the column configure itself
        final AtomicInteger columnIndex = new AtomicInteger(0);
        for (GenericColumn column : columnDescriptions) {
            if (column.getPostBuildListener() != null) {
                column.getPostBuildListener().accept(
                        dataCollection.stream().map(row -> row[columnIndex.get()]).toArray(String[]::new));
            }
            columnIndex.getAndIncrement();
        }

        if (columnDescriptions.stream().noneMatch(GenericColumn::isDoUse)) {
            throw new IllegalArgumentException("At least one column needs to return true on isDoUse");
        }

        if (columnSize != columnDescriptions.size()) {
            throw new IllegalArgumentException("InternalColumn count does not match loaded data's columns." +
                    "Please add one column description for each column that exists in the loaded file");
        }

        return TabularPreprocessor.createAnchorTabular(columnDescriptions, targetColumn, dataCollection, this.doBalance);
    }

    /**
     * Gets called when a row is removed by using excludeFirst.
     * <p>
     * Implementations may chose to use this information
     *
     * @param removedRow the removed row usually containing meta information, i.e., variable names
     */
    void registerRemovedRow(String[] removedRow) {
    }

    /**
     * Registers a target column
     *
     * @param column the target column to be set
     */
    void addInternalTargetColumn(GenericColumn column) {
        if (targetColumn != null)
            throw new IllegalArgumentException("Only one target column can be set");
        this.targetColumn = column;
    }

    /**
     * Returns all column descriptions.
     * <p>
     * There must be one column description for each column in the file
     *
     * @param fileColumnSize the number of columns that were found in the file.
     *                       This information allows builder implementations to perform required adaptations, e.g. in
     *                       case an ID has not been given for a column so that it can be ignored
     * @return all {@link GenericColumn}s, sorted by index
     */
    abstract Collection<GenericColumn> getColumnDescriptions(int fileColumnSize);


    /**
     * May be used to configure the preprocessor to balance the dataset, i.e. to truncate the set of instances so
     * that each label has the same amount of instances
     *
     * @param doBalance true, if to balance dataset
     * @return the current {@link AbstractTabularBuilder}'s instance
     */
    @SuppressWarnings("unchecked")
    public T setDoBalance(boolean doBalance) {
        this.doBalance = doBalance;
        return (T) this;
    }
}
