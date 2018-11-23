package de.viadee.anchorj.spark;

import de.viadee.anchorj.AnchorConstructionBuilder;
import de.viadee.anchorj.AnchorResult;
import de.viadee.anchorj.DataInstance;
import de.viadee.anchorj.global.BatchExplainer;
import org.apache.spark.SparkConf;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;

import java.util.List;

/**
 * Batch explainer offering distributed and global explanations.
 * <p>
 * As due to Spark's nature there are multiple requirements to work with serializable Objects, there have been multiple
 * obstacles while developing as not every class could be easily made serializable.
 *
 * @param <T> Type of the instance
 */
public class SparkBatchExplainer<T extends DataInstance<?>> implements BatchExplainer<T> {
    private static JavaSparkContext sc;

    /**
     * Instantiates the batch explainer
     *
     * @param sparkContext the spark context used for initialization. Use createSparkConf for a default configuration
     */
    public SparkBatchExplainer(JavaSparkContext sparkContext) {
        this.sc = sparkContext;
    }

    /**
     * Instantiates the batch explainer
     *
     * @param hadoopHomeDir the hadoop installation directory
     */
    public SparkBatchExplainer(String hadoopHomeDir) {
        this(createLocalSparkConf(hadoopHomeDir));
    }

    /**
     * May be used to create a default {@link JavaSparkContext}
     *
     * @param hadoopHomeDir the hadoop installation directory
     * @return a {@link JavaSparkContext} usable to instantiate this class
     */
    public static JavaSparkContext createLocalSparkConf(String hadoopHomeDir) {
        return SparkBatchExplainer.createSparkConf(hadoopHomeDir, "local[*]");
    }

    /**
     * May be used to create a default {@link JavaSparkContext}
     *
     * @param hadoopHomeDir the hadoop installation directory
     * @param master        the master url
     * @return a {@link JavaSparkContext} usable to instantiate this class
     */
    public static JavaSparkContext createSparkConf(String hadoopHomeDir, String master) {
        System.setProperty("hadoop.home.dir", hadoopHomeDir);
        // [*] configures Spark to use as many cores/threads as applicable
        SparkConf conf = new SparkConf().setMaster(master).setAppName(SparkBatchExplainer.class.getCanonicalName());
        return new JavaSparkContext(conf);
    }

    @Override
    public AnchorResult<T>[] obtainAnchors(AnchorConstructionBuilder<T> anchorConstructionBuilder, List<T> instances) {
        // Distribute the instances among the instances
        JavaRDD<T> parallelizedInstances = sc.parallelize(instances);

        List<AnchorResult<T>> result = parallelizedInstances
                .map(i -> AnchorConstructionBuilder.buildForSP(anchorConstructionBuilder, i).constructAnchor())
                .collect();

        // "Hacks" used to circumvent serializable issues
//        List<AnchorResult<T>> result = parallelizedInstances
//                .mapToPair(new ClassificationMapper<>(anchorConstructionBuilder.getClassificationFunction()))
//                .map(new ConstructionMapper<>(anchorConstructionBuilder))
//                .collect();

        // Close the spark context
        this.sc.close();

        @SuppressWarnings("unchecked") final AnchorResult<T>[] anchorResults = result
                .toArray((AnchorResult<T>[]) new AnchorResult[0]);
        return anchorResults;
    }


}
