package de.viadee.xai.anchor.adapter.tabular.util;

import java.io.Serializable;
import java.util.function.Supplier;

/**
 * {@link Supplier} interface which is {@link Serializable}
 */
@FunctionalInterface
public interface SerializableSupplier extends Supplier<Serializable>, Serializable {

}