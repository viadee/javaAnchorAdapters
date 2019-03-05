package de.viadee.xai.anchor.adapter.tabular.transformations;

import de.viadee.xai.anchor.adapter.tabular.util.SerializableSupplier;

import java.io.Serializable;

/**
 * Transformer allowing to easily swap null and empty string values with a defined value
 */
public class ReplaceEmptyTransformer implements Transformer {
    private static final long serialVersionUID = 6419476632394236169L;

    private final SerializableSupplier valueSupplier;

    /**
     * Instantiates the instance
     *
     * @param removeWith the value to replace null values with
     */
    public ReplaceEmptyTransformer(Serializable removeWith) {
        this(() -> removeWith);
    }

    /**
     * Instantiates the instance
     *
     * @param valueSupplier the supplier for a value
     */
    public ReplaceEmptyTransformer(SerializableSupplier valueSupplier) {
        this.valueSupplier = valueSupplier;
    }

    @Override
    public Serializable apply(Serializable object) {
        return (object == null || "".equals(object)) ? valueSupplier.get() : object;
    }
}
