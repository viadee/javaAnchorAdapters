package de.viadee.anchorj.tabular.transformations;

import java.util.function.Supplier;
import java.util.stream.Stream;

/**
 * Transformer allowing to easily swap null and empty string values with a defined value
 */
public class ReplaceNullTransformer implements Transformer {
    private final Supplier<Object> valueSupplier;

    /**
     * Instantiates the instance
     *
     * @param removeWith the value to replace null values with
     */
    public ReplaceNullTransformer(Object removeWith) {
        this(() -> removeWith);
    }

    /**
     * Instantiates the instance
     *
     * @param valueSupplier the supplier for a value
     */
    public ReplaceNullTransformer(Supplier<Object> valueSupplier) {
        this.valueSupplier = valueSupplier;
    }

    @Override
    public Object[] apply(Object[] objects) {
        return Stream.of(objects).map(o -> (o == null || "".equals(o)) ? valueSupplier.get() : o).toArray(Object[]::new);
    }
}
