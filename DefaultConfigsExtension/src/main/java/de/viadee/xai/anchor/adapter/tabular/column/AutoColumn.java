package de.viadee.xai.anchor.adapter.tabular.column;

import de.viadee.xai.anchor.adapter.tabular.discretizer.Discretizer;
import de.viadee.xai.anchor.adapter.tabular.transformations.Transformer;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

/**
 * TODO description
 */
public class AutoColumn extends GenericColumn {
    private static final long serialVersionUID = -7678371903171996032L;

    private final AtomicBoolean listenerCalled = new AtomicBoolean(false);
    private final Consumer<String[]> postBuildListener;

    /**
     * Instantiates the column
     *
     * @param name the column's name
     */
    public AutoColumn(String name) {
        // TODO link to other constructors
        super(name);
        this.postBuildListener = createPostBuildListener();
    }

    // TODO constructors


    private Consumer<String[]> createPostBuildListener() {
        return column -> {
            if (listenerCalled.get()) {
                throw new IllegalArgumentException("This column may only be initialized once");
            }
            listenerCalled.set(true);

            // TODO Listen and configure in here
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
        return super.getTransformers();
    }

    @Override
    public Discretizer getDiscretizer() {
        if (!listenerCalled.get()) {
            throw new IllegalArgumentException("This Column has not yet been initialized, " +
                    "as its listener was not called");
        }
        return super.getDiscretizer();
    }
}
