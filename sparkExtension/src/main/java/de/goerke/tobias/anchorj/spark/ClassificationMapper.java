package de.goerke.tobias.anchorj.spark;

import de.viadee.anchorj.ClassificationFunction;
import de.viadee.anchorj.DataInstance;
import org.apache.spark.api.java.function.PairFunction;
import scala.Tuple2;

/**
 * This class is used in conjunction with Apache Spark.
 * <p>
 * As Spark expects serializable objects, this "hack" is necessary when dealing with data that cannot easily be
 * serialized.
 * <p>
 * This class in particular maps each candidate to a tuple of candidate plus its prediction.
 *
 * @param <T> Type of the instance
 */
class ClassificationMapper<T extends DataInstance<?>> implements PairFunction<T, T, Integer> {
    private static ClassificationFunction classificationFunction;

    /**
     * @param classificationFunction the classificationFunction used to predict the label
     */
    @SuppressWarnings("static-access")
    ClassificationMapper(ClassificationFunction<T> classificationFunction) {
        this.classificationFunction = classificationFunction;
    }

    @Override
    @SuppressWarnings("unchecked")
    public Tuple2<T, Integer> call(T t) {
        return new Tuple2<>(t, classificationFunction.predict(t));
    }
}
