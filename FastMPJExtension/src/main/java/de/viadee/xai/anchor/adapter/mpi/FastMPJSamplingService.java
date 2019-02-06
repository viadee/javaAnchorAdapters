package de.viadee.xai.anchor.adapter.mpi;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import de.viadee.xai.anchor.algorithm.AnchorCandidate;
import de.viadee.xai.anchor.algorithm.ClassificationFunction;
import de.viadee.xai.anchor.algorithm.DataInstance;
import de.viadee.xai.anchor.algorithm.PerturbationFunction;
import de.viadee.xai.anchor.algorithm.execution.SamplingService;
import de.viadee.xai.anchor.algorithm.execution.SamplingSession;
import de.viadee.xai.anchor.algorithm.execution.sampling.DefaultSamplingFunction;
import de.viadee.xai.anchor.algorithm.execution.sampling.SamplingFunction;
import mpi.MPI;

/**
 * This SamplingServices implements MPI by using FastMPJ.
 * <p>
 * In order to use this service, please follow these steps:
 * <ol>
 * <li>Install FastMPJ. Environment variables need to be set correctly.</li>
 * <li>Create a main class calling the AnchorConstruction</li>
 * <li>Compile the main class</li>
 * <li>Go to its containing class folder. Usually, this is target/classes</li>
 * <li>Run fmpjrun. Note: classpath needs to be specified, e.g.
 * fmpjrun -np 1 -cp "&lt;absolute_path&gt;\classpath1859923920.jar"
 * -class de.viadee.anchorj.showcase.experiments.FastMPJExperiment</li>
 * </ol>
 * <p>
 * TODO publish experiments
 *
 * @param <T> Type of the {@link DataInstance}
 */
public class FastMPJSamplingService<T extends DataInstance<?>> extends FastMPJBaseClass implements SamplingService {
    private static final long serialVersionUID = -433861851523600400L;

    private static final Logger LOGGER = LoggerFactory.getLogger(FastMPJSamplingService.class);


    private static final int MESSAGE_SIZE_TAG = 0;
    private static final int COMMAND_SEND_TAG = 1;
    private static final int RESPOND_TAG = 2;

    private final SamplingFunction samplingFunction;

    private int totalSampleCount = 0;

    /**
     * Creates the MPI sampling service
     *
     * @param args                   String array containing the arguments passed at the main's call
     * @param classificationFunction the {@link ClassificationFunction}
     * @param perturbationFunction   the {@link PerturbationFunction}
     */
    public FastMPJSamplingService(final String[] args,
                                  final ClassificationFunction<T> classificationFunction,
                                  final PerturbationFunction<T> perturbationFunction) {
        this(FastMPJInitializer.initEnvironment(args), classificationFunction, perturbationFunction);
    }

    /**
     * Creates the MPI sampling service
     *
     * @param initializerResult      result of a self-initialized environment
     * @param classificationFunction the {@link ClassificationFunction}
     * @param perturbationFunction   the {@link PerturbationFunction}
     */
    public FastMPJSamplingService(int[] initializerResult, ClassificationFunction<T> classificationFunction,
                                  PerturbationFunction<T> perturbationFunction) {
        super(initializerResult);
        this.samplingFunction = new DefaultSamplingFunction<>(classificationFunction, perturbationFunction);
    }


    @Override
    protected void init() {
        while (!isIsFinalizeCalled()) {
            LOGGER.debug("Worker {} waiting for message size receive", me);
            int[] sizeAndLabelData = new int[2];
            MPI.COMM_WORLD.Recv(sizeAndLabelData, 0, sizeAndLabelData.length, MPI.INT, 0, MESSAGE_SIZE_TAG);
            int label = sizeAndLabelData[1];
            int[] data = new int[sizeAndLabelData[0]];
            MPI.COMM_WORLD.Recv(data, 0, data.length, MPI.INT, 0, COMMAND_SEND_TAG);
            int[] features = new int[data.length - 1];
            int sampleCount = data[data.length - 1];
            System.arraycopy(data, 0, features, 0, features.length);
            LOGGER.debug("Worker {} received operation to sample {} with label {} for {} times", me, features, label, sampleCount);

            Collection<Integer> featuresCollection = IntStream.of(features).boxed().collect(Collectors.toList());
            AnchorCandidate candidate = new AnchorCandidate(featuresCollection);
            samplingFunction.evaluate(candidate, sampleCount, label);

            MPI.COMM_WORLD.Send(new int[] { candidate.getSampledSize(), candidate.getPositiveSamples() }, 0, 2, MPI.INT, 0, RESPOND_TAG);

            LOGGER.debug("Worker {} finished one cycle", me);
        }
    }


    @Override
    public double getTimeSpentSampling() {
        return 0;
    }

    @Override
    public int getSamplesTakenCount() {
        return 0;
    }

    @Override
    public SamplingSession createSession(int explainedInstanceLabel) {
        if (!isInitialized())
            throw new IllegalArgumentException("initialize() must have been called before using the FastMPJ service");
        return new SamplingSession() {
            private static final long serialVersionUID = 1211871920035699406L;
            final Map<AnchorCandidate, Integer> samplingCountMap = new LinkedHashMap<>();

            @Override
            public SamplingSession registerCandidateEvaluation(AnchorCandidate candidate, int count) {
                if (samplingCountMap.containsKey(candidate))
                    count += samplingCountMap.get(candidate);
                samplingCountMap.put(candidate, count);

                return this;
            }

            @Override
            public void run() {
                if (me == 0) {
                    LOGGER.debug("Starting parent sampling process");

                    for (Map.Entry<AnchorCandidate, Integer> entry : samplingCountMap.entrySet()) {
                        final Integer[] features = entry.getKey().getOrderedFeatures().toArray(new Integer[0]);

                        final int samplesPerWorker = entry.getValue() / (nProcesses - 1);
                        int remaining = entry.getValue() % (nProcesses - 1);

                        for (int i = 1; i < nProcesses; i++) {
                            int[] featuresAndCount = new int[features.length + 1];
                            for (int j = 0; j < features.length; j++) {
                                featuresAndCount[j] = features[j];
                            }
                            int sampleCount = samplesPerWorker;
                            if (remaining > 0) {
                                sampleCount++;
                                remaining--;
                            }
                            featuresAndCount[featuresAndCount.length - 1] = sampleCount;
                            totalSampleCount += sampleCount;

                            MPI.COMM_WORLD.Isend(new int[] { featuresAndCount.length, explainedInstanceLabel }, 0, 2, MPI.INT, i, MESSAGE_SIZE_TAG);
                            MPI.COMM_WORLD.Isend(featuresAndCount, 0, featuresAndCount.length, MPI.INT, i, COMMAND_SEND_TAG);
                            LOGGER.debug("Successfully sent features to sample {} to worker {}", featuresAndCount, i);
                        }

                        LOGGER.debug("Waiting for responses in main thread");
                        for (int i = 1; i < nProcesses; i++) {
                            int[] samplingResult = new int[2];
                            MPI.COMM_WORLD.Recv(samplingResult, 0, samplingResult.length, MPI.INT, i, RESPOND_TAG);
                            LOGGER.debug("Worker {} sampled {} times, with {} positives", i, samplingResult[0], samplingResult[1]);
                            entry.getKey().registerSamples(samplingResult[0], samplingResult[1]);
                        }
                    }
                }
            }
        };
    }

    @Override
    public void endSampling() {
        // TODO should call finalize environment?
//        FastMPJInitializer.finalizeEnvironment();
    }

    @Override
    public SamplingService notifySamplingFunctionChange(SamplingFunction samplingFunction) {
        throw new UnsupportedOperationException("SubmodularPick is not available for FastMPJ single sampling services");
    }

    /**
     * @return the amount of overall sampled instances
     */
    public int getTotalSampleCount() {
        return totalSampleCount;
    }
}
