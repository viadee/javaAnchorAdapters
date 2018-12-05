package de.viadee.anchorj.tabular.column;

/**
 * A column to be ignored
 */
public class IgnoredColumn extends GenericColumn {

    /**
     * Instantiates the column
     *
     * @param name a name for reasons of clarity and comprehensibility
     */
    public IgnoredColumn(String name) {
        super(name, null, null);
    }

    /**
     * Instantiates the column
     */
    public IgnoredColumn() {
        super(null, null, null);
    }

    @Override
    public boolean isDoUse() {
        return false;
    }
}
