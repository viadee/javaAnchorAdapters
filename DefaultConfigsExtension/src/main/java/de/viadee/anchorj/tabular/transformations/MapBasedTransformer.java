package de.viadee.anchorj.tabular.transformations;

import java.io.Serializable;
import java.util.Map;

/**
 * A transformer applying values from a map.
 * <p>
 * Thus, maps all values that appear in the map's keys to respective value values
 */
public class MapBasedTransformer implements Transformer {
    private static final long serialVersionUID = -8869283260825890867L;

    private final Map<Serializable, Serializable> transformationMap;

    /**
     * Creates the instance
     *
     * @param transformationMap the map used for transforming the data
     */
    public MapBasedTransformer(final Map<Serializable, Serializable> transformationMap) {
        this.transformationMap = transformationMap;
    }

    @Override
    public Serializable[] apply(Serializable[] objects) {
        Serializable[] result = new Serializable[objects.length];
        for (int i = 0; i < result.length; i++) {
            Serializable transformValue = transformationMap.get(objects[i]);
            // Only apply transform if it is possible
            result[i] = (transformValue == null) ? objects[i] : transformValue;
        }
        return result;
    }
}
