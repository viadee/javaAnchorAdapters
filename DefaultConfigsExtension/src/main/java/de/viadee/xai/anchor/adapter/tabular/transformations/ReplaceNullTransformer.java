package de.viadee.xai.anchor.adapter.tabular.transformations;

import de.viadee.xai.anchor.adapter.tabular.util.SerializableSupplier;

import java.io.Serializable;

/**
 * Transformer allowing to easily swap null values with a defined value
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
        return (object == null) ? valueSupplier.get() : object;
    }
}
