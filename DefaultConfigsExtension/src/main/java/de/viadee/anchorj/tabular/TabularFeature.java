package de.viadee.anchorj.tabular;

import java.io.Serializable;

/**
 * Represents a de.viadee.anchorj.tabular feature, i.e. a column
 */
public class TabularFeature implements Serializable {
    private final ColumnType columnType;
    private final String name;
    private final boolean isTargetFeature;

    /**
     * Constructs the instance
     *
     * @param columnType      the type of the column
     * @param name            the name of the column
     * @param isTargetFeature true, if the target feature, i.e. label
     */
    TabularFeature(ColumnType columnType, String name, boolean isTargetFeature) {
        this.columnType = columnType;
        this.name = name;
        this.isTargetFeature = isTargetFeature;
    }

    /**
     * @return the column type
     */
    public ColumnType getColumnType() {
        return columnType;
    }

    /**
     * @return the name of the column
     */
    public String getName() {
        return name;
    }

    /**
     * @return true, if target feature. Must be unique in column set
     */
    public boolean isTargetFeature() {
        return isTargetFeature;
    }

    @Override
    public String toString() {
        return "TabularFeature{" +
                "columnType=" + columnType +
                ", name='" + name + '\'' +
                '}';
    }

    /**
     * Represents the type of a column - whether the contained data is categorical or nominal
     */
    public enum ColumnType {
        CATEGORICAL, NOMINAL, NATIVE
    }
}
