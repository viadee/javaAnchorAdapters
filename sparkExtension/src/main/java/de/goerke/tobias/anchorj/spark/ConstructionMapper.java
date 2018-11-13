package de.goerke.tobias.anchorj.spark;

import de.goerke.tobias.anchorj.AnchorConstructionBuilder;
import de.goerke.tobias.anchorj.AnchorResult;
import de.goerke.tobias.anchorj.DataInstance;
import org.apache.spark.api.java.function.Function;
import scala.Tuple2;


/**
 * This class is used in conjunction with Apache Spark.
 * <p>
 * As Spark expects serializable objects, this "hack" is necessary when dealing with data that cannot easily be
 * serialized.
 * <p>
 * This class in particular executes the anchor construction for a tuple generated by the {@link ClassificationMapper}
 *
 * @param <T> Type of the instance
 */
class ConstructionMapper<T extends DataInstance<?>> implements Function<Tuple2<T, Integer>, AnchorResult<T>> {
    private static AnchorConstructionBuilder anchorConstructionBuilder;

    /**
     * Instantiates a new Construction mapper.
     *
     * @param anchorConstructionBuilder the builder used to construct an
     *                                  {@link de.goerke.tobias.anchorj.AnchorConstruction} instance
     */
    @SuppressWarnings("static-access")
    ConstructionMapper(AnchorConstructionBuilder<T> anchorConstructionBuilder) {
        this.anchorConstructionBuilder = anchorConstructionBuilder;
    }

    @Override
    @SuppressWarnings("unchecked")
    public AnchorResult<T> call(Tuple2<T, Integer> tIntegerTuple2) {
        return anchorConstructionBuilder.setupForSP(tIntegerTuple2._1, tIntegerTuple2._2).build().constructAnchor();
    }
}
