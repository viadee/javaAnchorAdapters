package de.viadee.xai.anchor.adapter.tabular;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

/**
 *
 */
public class DataFrame {

    private final LinkedList<GenericColumn> columns;

    private Serializable[][] dataFrame;

    public DataFrame(GenericColumn[] columns, Collection<String[]> data) {
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

    public DataFrame(GenericColumn[] columns, Serializable[][] dataFrame) {
        this.columns = new LinkedList<>(Arrays.asList(columns));
        this.dataFrame = dataFrame;
    }

    public LinkedList<GenericColumn> getColumns() {
        return this.columns;
    }

    public int getColumnIndex(final String featureName) {
        return this.columns.indexOf(getColumnInfo(featureName));
    }

    public int getColumnIndex(final GenericColumn column) {
        return this.columns.indexOf(column);
    }

    public GenericColumn getColumnInfo(final String featureName) {
        return this.columns.stream().filter((genericColumn -> genericColumn.getName().equals(featureName))).findFirst()
                .orElseThrow(() -> new IllegalArgumentException("No feature with name " + featureName + " found"));
    }

    public Serializable[] getColumn(String featureName) {
        return getColumn(this.getColumnIndex(featureName));
    }

    public Serializable[] getColumn(GenericColumn feature) {
        return getColumn(this.getColumnIndex(feature));
    }

    private Serializable[] getColumn(int featureIndex) {
        return this.dataFrame[featureIndex];
    }

    public Serializable[] removeColumn(String featureName) {
        return removeColumn(getColumnInfo(featureName));
    }

    public Serializable[] removeColumn(GenericColumn feature) {
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

    public void transformColumn(String featureName, Transformer transform) {
        transformColumn(this.getColumnIndex(featureName), transform);
    }

    public void transformColumn(GenericColumn feature, Transformer transform) {
        transformColumn(this.getColumnIndex(feature), transform);
    }

    private void transformColumn(int featureIndex, Transformer transform) {
        this.dataFrame[featureIndex] = transform.apply(this.dataFrame[featureIndex]);
    }

    public Integer[] discretizeColumn(String featureName, Discretizer discretizer) {
        return discretizeColumn(this.getColumnIndex(featureName), discretizer);
    }

    public Integer[] discretizeColumn(GenericColumn feature, Discretizer discretizer) {
        return discretizeColumn(this.getColumnIndex(feature), discretizer);
    }

    private Integer[] discretizeColumn(int featureIndex, Discretizer discretizer) {
        return discretizer.apply(this.dataFrame[featureIndex]);
    }

    public int getNCols() {
        return this.columns.size();
    }

    public int getNRows() {
        return this.dataFrame[0].length;
    }

    public Serializable[] getRow(int rowIndex) {
        Serializable[] row = new Serializable[getNCols()];
        for (int i = 0; i < row.length; i++) {
            row[i] = this.dataFrame[i][rowIndex];
        }

        return row;
    }

}
