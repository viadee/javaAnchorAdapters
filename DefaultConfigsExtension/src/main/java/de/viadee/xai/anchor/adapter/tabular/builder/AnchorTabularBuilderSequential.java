package de.viadee.xai.anchor.adapter.tabular.builder;

import de.viadee.xai.anchor.adapter.tabular.AnchorTabular;
import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.column.IgnoredColumn;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

/**
 * Used to sequentially construct an {@link AnchorTabular} instance.
 * <p>
 * For each column contained in the input one column description must be defined.
 * <p>
 * As this mode is sequential, the ordering of the called add operations matters.
 * <p>
 * The first addColumn / addIgnoredColumn will be assigned to column 0, the second to column 1 and so forth
 */
public class AnchorTabularBuilderSequential extends AbstractTabularBuilder {
    private final List<GenericColumn> columnDescriptions = new ArrayList<>();

    /**
     * Constructs the builder
     */
    public AnchorTabularBuilderSequential() {
    }

    /**
     * Registers a column
     *
     * @param column the column to be added
     * @return the {@link AnchorTabularBuilderSequential} instance
     */
    public AnchorTabularBuilderSequential addColumn(GenericColumn column) {
        this.columnDescriptions.add(column);
        return this;
    }

    /**
     * Marks a column to be ignored
     *
     * @return the {@link AnchorTabularBuilderSequential}
     */
    public AnchorTabularBuilderSequential addIgnoredColumn() {
        return addIgnoredColumn("ignored" + UUID.randomUUID());
    }

    /**
     * Marks a column to be ignored
     *
     * @param name a name for reasons of clarity and comprehensibility
     * @return the {@link AnchorTabularBuilderSequential}
     */
    public AnchorTabularBuilderSequential addIgnoredColumn(String name) {
        this.columnDescriptions.add(new IgnoredColumn(name));
        return this;
    }

    /**
     * Marks a column to be ignored
     *
     * @param column a column for reasons of clarity and comprehensibility
     * @return the {@link AnchorTabularBuilderSequential}
     */
    public AnchorTabularBuilderSequential addIgnoredColumn(GenericColumn column) {
        this.columnDescriptions.add(new IgnoredColumn(column.getName()));
        return this;
    }

    /**
     * Registers a target column
     *
     * @param column the target column to be set
     * @return the {@link AnchorTabularBuilderSequential} instance
     */
    public AnchorTabularBuilderSequential addTargetColumn(GenericColumn column) {
        addInternalTargetColumn(column);
        this.columnDescriptions.add(column);
        return this;
    }

    @Override
    Collection<GenericColumn> getColumnDescriptions(int fileColumnSize) {
        return columnDescriptions;
    }
}
