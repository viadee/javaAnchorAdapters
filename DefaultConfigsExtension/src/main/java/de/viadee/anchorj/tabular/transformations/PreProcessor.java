package de.viadee.anchorj.tabular.transformations;

import java.io.Serializable;
import java.util.Objects;
import java.util.function.Function;

/**
 *
 */
@SuppressWarnings("WeakerAccess")
public abstract class PreProcessor implements Function<Serializable[][], Serializable[][]>, Serializable {
    private static final long serialVersionUID = 3269540580048583929L;

    private Integer processColumn;

    public PreProcessor(Integer processColumn) {
        this.processColumn = processColumn;
    }

    public Integer getProcessColumn() {
        return processColumn;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PreProcessor that = (PreProcessor) o;
        return Objects.equals(processColumn, that.processColumn);
    }

    @Override
    public int hashCode() {
        return Objects.hash(processColumn);
    }

    @Override
    public String toString() {
        return "PreProcessor{" +
                "processColumn=" + processColumn +
                '}';
    }
}
