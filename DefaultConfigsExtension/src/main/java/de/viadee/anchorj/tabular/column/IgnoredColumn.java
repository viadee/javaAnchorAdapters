package de.viadee.anchorj.tabular.column;

/**
 * A column to be ignored
 */
public class IgnoredColumn extends GenericColumn {
    private static final long serialVersionUID = 83367999246693636L;

    /**
     * Instantiates the column
     *
     * @param name a name for reasons of clarity and comprehensibility
     */
    public IgnoredColumn(String name, int originalColumnIndex) {
        super(name, originalColumnIndex, null, null);
    }

    /**
     * Instantiates the column
     */
    public IgnoredColumn(int originalColumnIndex) {
        super(null, originalColumnIndex, null, null);
    }

    @Override
    public boolean isDoUse() {
        return false;
    }
}
