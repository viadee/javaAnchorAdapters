package de.viadee.xai.anchor.adapter.global;

import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import org.apache.commons.lang3.time.StopWatch;
import org.apache.spark.SparkConf;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import de.viadee.xai.anchor.algorithm.AnchorConstruction;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.DataInstance;
import de.viadee.xai.anchor.algorithm.global.BatchExplainer;

/**
 * Batch explainer offering distributed and global explanations.
 * <p>
 * As due to Spark's nature there are multiple requirements to work with serializable Objects, there have been multiple
 * obstacles while developing as not every class could be easily made serializable.
 *
 * @param <T> Type of the instance
 */
public class SparkBatchExplainer<T extends DataInstance<?>> implements BatchExplainer<T> {
    private static final long serialVersionUID = 7167566798608926736L;

    private static final Logger LOGGER = LoggerFactory.getLogger(SparkBatchExplainer.class);
    private final JavaSparkContext sc;

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


        LOGGER.info("Values {} {}", sc.defaultParallelism(), sc.defaultMinPartitions());
        final List<AnchorConstruction<T>> constructors = instances.stream()
                .map(t -> AnchorConstructionBuilder.buildForSP(anchorConstructionBuilder, t))
                .collect(Collectors.toList());
        StopWatch stopWatch = StopWatch.createStarted();
        LOGGER.info("============================================1");
        LOGGER.info("============================================1");
        LOGGER.info("============================================1");
        List<AnchorResult<T>> result = sc.parallelize(constructors).cache().map(AnchorConstruction::constructAnchor).cache().collect();
        LOGGER.info("============================================1");
        LOGGER.info("============================================1");
        LOGGER.info("============================================1");
        LOGGER.info("1 Took {}", stopWatch.getTime(TimeUnit.MILLISECONDS));
        LOGGER.info("============================================2");
        LOGGER.info("============================================2");
        LOGGER.info("============================================2");
        result = sc.parallelize(constructors).cache().map(AnchorConstruction::constructAnchor).cache().collect();


//        LOGGER.info("============================================2");
//        LOGGER.info("============================================2");
//        LOGGER.info("============================================2");
//
////
//        LOGGER.info("Distributing the instances among the workers");
//        final JavaRDD<AnchorConstructionBuilder<T>> b = sc.parallelize(Collections.singletonList(anchorConstructionBuilder)).cache();
//        JavaRDD<T> parallelizedInstances = sc.parallelize(instances).persist(StorageLevel.MEMORY_AND_DISK());
//        LOGGER.info("Distribution completed");
//

        stopWatch = StopWatch.createStarted();
        final JavaRDD<T> cache = sc.parallelize(instances).cache();
        LOGGER.info("============================================2");
        LOGGER.info("============================================2");
        LOGGER.info("============================================2");
        result = cache.map(i -> AnchorConstructionBuilder.buildForSP(anchorConstructionBuilder, i)
                .constructAnchor()).cache().collect();
        LOGGER.info("============================================2");
        LOGGER.info("============================================2");
        LOGGER.info("============================================2");
        LOGGER.info("2 Took {}", stopWatch.getTime(TimeUnit.MILLISECONDS));


        // Close the spark context
        LOGGER.info("Completed distributed computing. Closing Spark Context..");
        this.sc.close();

        @SuppressWarnings("unchecked") final AnchorResult<T>[] anchorResults = result
                .toArray((AnchorResult<T>[]) new AnchorResult[0]);
        return anchorResults;
    }

}
