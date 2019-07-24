package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.stream.Stream;

/**
 * This class implements a "smart" column which automatically infers the best settings for a given dataset
 * <p>
 * A column's tasks:
 * <ol>
 * <li>Define transformations to clean and prepare data and to translate String inputs to correct datatypes</li>
 * <li>Define discretizations which anchors can work with</li>
 * </ol>
 * <p>
 * This AutoColumn aims to automize these tasks by replacing missing values by null, detecting a column's datatype and
 * choosing an appropriate discretization approach.
 *
 * TODO work to be done for discretization tasks
 */
public class AutoColumn extends GenericColumn {
    private static final long serialVersionUID = -7678371903171996032L;

    private final AtomicBoolean listenerCalled = new AtomicBoolean(false);
    private final Consumer<String[]> postBuildListener;

    private GenericColumn internalColumn;

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public AutoColumn(String name) {
        super(name);
        this.postBuildListener = createPostBuildListener();
    }

    private Consumer<String[]> createPostBuildListener() {
        return column -> {
            if (listenerCalled.get()) {
                throw new IllegalArgumentException("This column may only be initialized once");
            }
            listenerCalled.set(true);

            final String[] nonEmptyRows = Stream.of(column)
                    .filter(Objects::nonNull)
                    .filter(s -> !s.isEmpty())
                    .toArray(String[]::new);

            if (Stream.of(nonEmptyRows).allMatch(row -> row.matches("^-?\\d+$"))) {
                this.internalColumn = IntegerColumn.fromStringInput(this.getName(), 5);
            } else if (Stream.of(nonEmptyRows).allMatch(row -> row.matches("^-?\\d*\\.\\d+$"))) {
                this.internalColumn = DoubleColumn.fromStringInput(this.getName(), 5);
            } else if (Stream.of(nonEmptyRows).allMatch(row -> row.toLowerCase().matches("^true|false$"))) {
                this.internalColumn = BooleanColumn.fromStringInput(this.getName());
            } else {
                this.internalColumn = new StringColumn(this.getName());
            }

        };
    }

    @Override
    public Consumer<String[]> getPostBuildListener() {
        return postBuildListener;
    }

    @Override
    public List<Transformer> getTransformers() {
        if (!listenerCalled.get()) {
            throw new IllegalArgumentException("This Column has not yet been initialized, " +
                    "as its listener was not called");
        }
        return this.internalColumn.getTransformers();
    }

    @Override
    public Discretizer getDiscretizer() {
        if (!listenerCalled.get()) {
            throw new IllegalArgumentException("This Column has not yet been initialized, " +
                    "as its listener was not called");
        }
        return this.internalColumn.getDiscretizer();
    }

    /**
     * @return the internally used column
     */
    public GenericColumn getInternalColumn() {
        return internalColumn;
    }
}
