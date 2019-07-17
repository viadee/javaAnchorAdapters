package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

/**
 * Constructs a column-based data frame to be used by preprocessing steps
 * <p>
 * Mutable object!
 */
class DataFrame {
    private final LinkedList<GenericColumn> columns;
    private Serializable[][] dataFrame;

    /**
     * Constructs the instance
     *
     * @param columns the columns to use
     * @param data    the actual data
     */
    DataFrame(GenericColumn[] columns, Collection<String[]> data) {
        final int nRows = data.size();
        final int nCols = columns.length;

        this.columns = new LinkedList<>(Arrays.asList(columns));
        this.dataFrame = new Serializable[nCols][];
        for (int i = 0; i < nCols; i++) {
            this.dataFrame[i] = new Serializable[nRows];
        }

        int rowIndex = 0;
        for (String[] row : data) {
            for (int column = 0; column < nCols; column++) {
                dataFrame[column][rowIndex] = row[column];
            }
            rowIndex++;
        }
    }

    /**
     * Constructs the instance
     *
     * @param columns   the columns to use
     * @param dataFrame the actual data
     */
    DataFrame(GenericColumn[] columns, Serializable[][] dataFrame) {
        this.columns = new LinkedList<>(Arrays.asList(columns));
        this.dataFrame = dataFrame;
    }

    /**
     * Return the internally stored column descriptions
     *
     * @return all stored columns
     */
    LinkedList<GenericColumn> getColumns() {
        return this.columns;
    }

    /**
     * Returns the column index
     *
     * @param column the {@link GenericColumn}
     * @return the internally used index of the column
     */
    int getColumnIndex(final GenericColumn column) {
        return this.columns.indexOf(column);
    }

    private GenericColumn getColumnInfo(final String featureName) {
        return this.columns.stream().filter((genericColumn -> genericColumn.getName().equals(featureName))).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No feature with name " + featureName + " found"));
    }

    /**
     * Returns the column
     *
     * @param feature the feature to obtain the column for
     * @return the column's values
     */
    Serializable[] getColumn(GenericColumn feature) {
        return getColumn(this.getColumnIndex(feature));
    }

    private Serializable[] getColumn(int featureIndex) {
        return this.dataFrame[featureIndex];
    }

    /**
     * Removes a column and its values
     *
     * @param featureName the feature to remove
     * @return the removed column
     */
    Serializable[] removeColumn(String featureName) {
        return removeColumn(getColumnInfo(featureName));
    }

    /**
     * Removes a column and its values
     *
     * @param feature the feature to remove
     * @return the removed column
     */
    Serializable[] removeColumn(GenericColumn feature) {
        if (feature == null) {
            throw new IllegalArgumentException("feature is null");
        }
        final int columnIndexToRemove = this.columns.indexOf(feature);
        if (!this.columns.remove(feature)) {
            throw new IllegalArgumentException("Feature " + feature + " not in list");
        }

        Serializable[][] newDataFrame = new Serializable[this.dataFrame.length - 1][];
        int newColumnIndexes = 0;
        Serializable[] removedColumn = null;
        for (int i = 0; i < this.dataFrame.length; i++) {
            if (i == columnIndexToRemove) {
                removedColumn = this.dataFrame[i];
                continue;
            }
            newDataFrame[newColumnIndexes] = this.dataFrame[i];
            newColumnIndexes++;
        }

        this.dataFrame = newDataFrame;
        return removedColumn;
    }

    /**
     * Applies a transformation to a column
     *
     * @param feature   the feature to transform the column for
     * @param transform the transformed column
     */
    void transformColumn(GenericColumn feature, Transformer transform) {
        transformColumn(this.getColumnIndex(feature), transform);
    }

    private void transformColumn(int featureIndex, Transformer transform) {
        this.dataFrame[featureIndex] = transform.apply(this.dataFrame[featureIndex]);
    }

    /**
     * Returns the discretized column
     *
     * @param feature     the column to use
     * @param discretizer the discretizer to use
     * @return the column as a discretized int array
     */
    Integer[] discretizeColumn(GenericColumn feature, Discretizer discretizer) {
        return discretizeColumn(this.getColumnIndex(feature), discretizer);
    }

    private Integer[] discretizeColumn(int featureIndex, Discretizer discretizer) {
        return discretizer.apply(this.dataFrame[featureIndex]);
    }

    /**
     * @return the column count
     */
    int getColumnCount() {
        return this.columns.size();
    }

    /**
     * @return the row count
     */
    int getRowCount() {
        return this.dataFrame[0].length;
    }

    /**
     * @param rowIndex the index to obtain the row for
     * @return the row as an object array
     */
    Serializable[] getRow(int rowIndex) {
        Serializable[] row = new Serializable[getColumnCount()];
        for (int i = 0; i < row.length; i++) {
            row[i] = this.dataFrame[i][rowIndex];
        }

        return row;
    }

}
