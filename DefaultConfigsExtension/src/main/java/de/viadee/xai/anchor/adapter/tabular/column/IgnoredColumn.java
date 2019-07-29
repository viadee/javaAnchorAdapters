package de.viadee.xai.anchor.adapter.tabular.column;

/**
 * A column to be ignored
 */
public class IgnoredColumn extends GenericColumn {
    private static final long serialVersionUID = 83367999246693636L;

    /**
     * Instantiates the column
     */
    public IgnoredColumn() {
        this(null);
    }

    /**
     * Instantiates the column
     *
     * @param name a name for reasons of clarity and comprehensibility
     */
    public IgnoredColumn(String name) {
        super(name, null, null);
    }

    @Override
    public boolean isDoUse() {
        return false;
    }
}
