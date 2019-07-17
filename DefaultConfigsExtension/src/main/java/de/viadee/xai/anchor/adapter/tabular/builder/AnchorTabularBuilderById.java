package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IgnoredColumn;

import java.util.*;
import java.util.stream.IntStream;

/**
 * Used to construct an {@link AnchorTabular} instance.
 * <p>
 * Assigns a {@link GenericColumn} to every column that exists in the dataset.
 * <p>
 * For each column that is not included, an {@link IgnoredColumn} is added
 */
public class AnchorTabularBuilderById extends AbstractTabularBuilder {
    private final SortedMap<Integer, GenericColumn> columnDescriptions = new TreeMap<>();

    /**
     * Constructs the builder
     */
    public AnchorTabularBuilderById() {
    }

    /**
     * Registers a column
     *
     * @param index  the index the column in located in the file
     * @param column the column to be added
     * @return the {@link AnchorTabular} instance
     */
    public AnchorTabularBuilderById addColumn(int index, GenericColumn column) {
        this.columnDescriptions.put(index, column);
        return this;
    }

    /**
     * Marks a column to be ignored
     *
     * @param index the index the column in located in the file
     * @return the {@link AnchorTabular.Builder}
     */
    public AnchorTabularBuilderById addIgnoredColumn(int index) {
        return addIgnoredColumn(index, "ignored" + index);
    }

    /**
     * Marks a column to be ignored
     *
     * @param index the index the column in located in the file
     * @param name  a name for reasons of clarity and comprehensibility
     * @return the {@link AnchorTabular.Builder}
     */
    public AnchorTabularBuilderById addIgnoredColumn(int index, String name) {
        this.columnDescriptions.put(index, new IgnoredColumn(name));
        return this;
    }

    /**
     * Marks a column to be ignored
     *
     * @param index  the index the column in located in the file
     * @param column a column for reasons of clarity and comprehensibility
     * @return the {@link AnchorTabular.Builder}
     */
    public AnchorTabularBuilderById addIgnoredColumn(int index, GenericColumn column) {
        this.columnDescriptions.put(index, new IgnoredColumn(column.getName()));
        return this;
    }

    /**
     * Registers a target column
     *
     * @param index  the index the column in located in the file
     * @param column the target column to be set
     * @return the {@link AnchorTabular} instance
     */
    public AnchorTabularBuilderById addTargetColumn(int index, GenericColumn column) {
        addInternalTargetColumn(column);
        this.columnDescriptions.put(index, column);
        return this;
    }

    @Override
    Collection<GenericColumn> getColumnDescriptions(int fileColumnSize) {
        IntStream.range(0, fileColumnSize).filter(i -> !columnDescriptions.keySet().contains(i)).
                forEach(this::addIgnoredColumn);
        return columnDescriptions.values();
    }
}
