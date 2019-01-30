package de.viadee.xai.anchor.adapter.tabular.transformations;

import java.io.Serializable;
import java.util.function.Supplier;

/**
 * Transformer allowing to easily swap null and empty string values with a defined value
 */
public class ReplaceNullTransformer implements Transformer {
    private static final long serialVersionUID = 6419476632394236169L;

    private final SerializableSupplier valueSupplier;

    /**
     * Instantiates the instance
     *
     * @param removeWith the value to replace null values with
     */
    public ReplaceNullTransformer(Serializable removeWith) {
        this(() -> removeWith);
    }

    /**
     * Instantiates the instance
     *
     * @param valueSupplier the supplier for a value
     */
    public ReplaceNullTransformer(SerializableSupplier valueSupplier) {
        this.valueSupplier = valueSupplier;
    }

    @Override
    public Serializable apply(Serializable object) {
        return (object == null || "".equals(object)) ? valueSupplier.get() : object;
    }

    @FunctionalInterface
    public interface SerializableSupplier extends Supplier<Serializable>, Serializable {

    }

}
