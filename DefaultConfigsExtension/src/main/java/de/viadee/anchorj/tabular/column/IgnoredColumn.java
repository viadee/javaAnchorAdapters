package de.viadee.anchorj.tabular.column;

/**
 * A column to be ignored
 */
public class IgnoredColumn extends AbstractColumn<Object> {
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
