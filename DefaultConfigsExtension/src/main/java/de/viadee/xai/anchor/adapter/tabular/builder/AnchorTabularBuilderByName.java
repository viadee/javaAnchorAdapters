package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IgnoredColumn;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Used to construct an {@link AnchorTabular} instance.
 * <p>
 * Assigns a {@link GenericColumn} to every column that exists in the dataset.
 * <p>
 * The columns that get added are matched by their name to the dataset. Therefore, a header containing variable names
 * must exist.
 * <p>
 * For each column that is not included, an {@link IgnoredColumn} is added
 */
public class AnchorTabularBuilderByName extends AbstractTabularBuilder {
    private final Map<String, GenericColumn> columnDescriptions = new LinkedHashMap<>();

    private String[] headers;

    /**
     * Constructs the builder
     */
    public AnchorTabularBuilderByName() {
    }

    /**
     * Registers a column that gets matched by its name
     *
     * @param column the column to be added
     * @return the {@link AnchorTabular} instance
     */
    public AnchorTabularBuilderByName addColumn(GenericColumn column) {
        if (column.getName() == null || column.getName().length() == 0)
            throw new IllegalArgumentException("A column name needs to be present");
        if (columnDescriptions.keySet().contains(column.getName())) {
            throw new IllegalArgumentException("Another column has already been registered containing the same name");
        }
        this.columnDescriptions.put(column.getName(), column);
        return this;
    }

    /**
     * Registers a target column
     *
     * @param column the target column to be set
     * @return the {@link AnchorTabular} instance
     */
    public AnchorTabularBuilderByName addTargetColumn(GenericColumn column) {
        addInternalTargetColumn(column);
        return addColumn(column);
    }

    @Override
    Collection<GenericColumn> getColumnDescriptions(int fileColumnSize) {
        HashSet<String> unassignedHeaders = columnDescriptions.values().stream().map(GenericColumn::getName)
                .collect(Collectors.toCollection(HashSet::new));
        unassignedHeaders.removeAll(Arrays.asList(headers));
        if (unassignedHeaders.size() > 0) {
            throw new IllegalArgumentException("Not all specified column names were found in the header list");
        }

        // Using this snippet causes the ordering to be as it is in the original document
        Collection<GenericColumn> columns = new ArrayList<>();
        for (String header : headers) {
            GenericColumn column = columnDescriptions.get(header);
            if (column == null)
                column = new IgnoredColumn(header);
            columns.add(column);
        }
        return columns;
    }

    @Override
    public AnchorTabular build(Collection<String[]> dataCollection, boolean excludeFirst) {
        if (excludeFirst) {
            throw new IllegalArgumentException("A header must be included when adding columns by name");
        }
        // We are passing true nonetheless so that the row gets removed. We will obtain the values in registerRemovedRow
        return super.build(dataCollection, true);
    }

    @Override
    void registerRemovedRow(String[] removedRow) {
        if (Stream.of(removedRow).distinct().count() != removedRow.length) {
            throw new IllegalArgumentException("There must be no duplicate variable names");
        }
        headers = removedRow;
        super.registerRemovedRow(removedRow);
    }
}
