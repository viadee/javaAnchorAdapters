package de.viadee.anchorj.spark;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Output;
import de.viadee.anchorj.AnchorConstructionBuilder;
import de.viadee.anchorj.AnchorResult;
import de.viadee.anchorj.DataInstance;
import de.viadee.anchorj.global.BatchExplainer;
import org.apache.spark.SparkConf;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.serializer.KryoSerializer;
import scala.Tuple2;

import java.io.ByteArrayOutputStream;
import java.util.List;

/**
 * Batch explainer offering distributed and global explanations.
 * <p>
 * As due to Spark's nature there are multiple requirements to work with serializable Objects, there have been multiple
 * obstacles while developing as not every class could be easily made serializable.
 * <p>
 * The solution was to create named classes holding static instances so that instances do not need to be serialized.
 * See {@link ClassificationMapper} and {@link ConstructionMapper}.
 *
 * @param <T> Type of the instance
 */
public class SparkBatchExplainer<T extends DataInstance<?>> implements BatchExplainer<T> {
    private static JavaSparkContext sc;

    /**
     * May be used to create a default {@link JavaSparkContext}
     *
     * @param hadoopHomeDir the hadoop installation directory
     * @return a {@link JavaSparkContext} usable to instantiate this class
     */
    public static JavaSparkContext createSparkConf(String hadoopHomeDir) {
        System.setProperty("hadoop.home.dir", hadoopHomeDir);
        // [*] configures Spark to use as many cores/threads as applicable
        SparkConf conf = new SparkConf().setMaster("local[*]").setAppName(SparkBatchExplainer.class.getCanonicalName());
        return new JavaSparkContext(conf);
    }

    /**
     * Instantiates the batch explainer
     *
     * @param sparkContext the spark context used for initialization. Use createSparkConf for a default configuration
     */
    public SparkBatchExplainer(JavaSparkContext sparkContext) {
        this.sc = sparkContext;
    }

    @Override
    public AnchorResult<T>[] obtainAnchors(AnchorConstructionBuilder<T> anchorConstructionBuilder, List<T> instances) {
        // Distribute the instances among the instances
        JavaRDD<T> parallelizedInstances = sc.parallelize(instances);

        final KryoSerializer kryoSerializer = new KryoSerializer(sc.getConf());
        final Kryo kryo = kryoSerializer.newKryo();
        final ByteArrayOutputStream bao = new ByteArrayOutputStream();
        final Output output = kryoSerializer.newKryoOutput();
        output.setOutputStream(bao);
        kryo.writeClassAndObject(output, anchorConstructionBuilder);
        output.close();

        List<AnchorResult<T>> result = parallelizedInstances
                .mapToPair(i -> new Tuple2<>(i, anchorConstructionBuilder.getClassificationFunction().predict(i)))
                .map(p -> anchorConstructionBuilder.setupForSP(p._1, p._2).build().constructAnchor())
                .collect();

        // "Hacks" used to circumvent serializable issues
//        List<AnchorResult<T>> result = parallelizedInstances
//                .mapToPair(new ClassificationMapper<>(anchorConstructionBuilder.getClassificationFunction()))
//                .map(new ConstructionMapper<>(anchorConstructionBuilder))
//                .collect();
        // Close the spark context
        // this.sc.close();

        @SuppressWarnings("unchecked") final AnchorResult<T>[] anchorResults = result
                .toArray((AnchorResult<T>[]) new AnchorResult[0]);
        return anchorResults;
    }
}
