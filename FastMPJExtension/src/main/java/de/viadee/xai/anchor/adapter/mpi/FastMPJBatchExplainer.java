package de.viadee.xai.anchor.adapter.mpi;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.DataInstance;
import de.viadee.xai.anchor.algorithm.global.BatchExplainer;

/**
 * This BatchExplainer implements MPI by using FastMPJ.
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
 * TODO publish experiments
 *
 * @param <T> Type of the {@link DataInstance}
 */
public class FastMPJBatchExplainer<T extends DataInstance<?>> extends FastMPJBaseClass implements BatchExplainer<T> {
    private static final long serialVersionUID = -8317831577640300990L;

    private static final Logger LOGGER = LoggerFactory.getLogger(FastMPJBatchExplainer.class);
    private static final int READY_TAG = 0;
    private static final int SEND_ELEMENT_TAG = 1;
    private static final int RECEIVE_ELEMENT_TAG = 2;

    private final AnchorConstructionBuilder<T> builder;

    /**
     * Creates the MPI sampling service
     *
     * @param builder the builder used for building construction instances
     * @param args    String array containing the arguments passed at the main's call
     */
    public FastMPJBatchExplainer(AnchorConstructionBuilder<T> builder, String[] args) {
        super(args);
        this.builder = builder;
    }

    /**
     * Creates the MPI sampling service
     *
     * @param builder           the builder used for building construction instances
     * @param initializerResult result of a self-initialized environment
     */
    public FastMPJBatchExplainer(AnchorConstructionBuilder<T> builder, int[] initializerResult) {
        super(initializerResult);
        this.builder = builder;
    }

    @Override
    public AnchorResult<T>[] obtainAnchors(AnchorConstructionBuilder<T> anchorConstructionBuilder, List<T> instances) {
        if (me != 0)
            throw new IllegalArgumentException("Should only be called by root");

        List<T> remaining = Collections.synchronizedList(new ArrayList<>(instances));
        List<AnchorResult<T>> result = Collections.synchronizedList(new ArrayList<>());

        final List<Thread> threads = new ArrayList<>();
        for (int i = 1; i < nProcesses; i++) {
            final int threadIndex = i;
            threads.add(new Thread(() -> {
                LOGGER.info("Starting communication thread #{}", threadIndex);
                while (!isIsFinalizeCalled()) {
                    FastMPJUtil.waitForSignal(threadIndex, READY_TAG);
                    LOGGER.info("Received ready signal in com thread #{}", threadIndex);

                    T element;
                    try {
                        element = remaining.remove(0);
                    } catch (IndexOutOfBoundsException e) {
                        LOGGER.info("No more element, finishing com thread #{}", threadIndex);
                        return;
                    }

                    LOGGER.info("Sending element in com threa #{}", threadIndex);
                    FastMPJUtil.send(element, threadIndex, SEND_ELEMENT_TAG);
                    AnchorResult<T> elementResult = FastMPJUtil.receive(threadIndex, RECEIVE_ELEMENT_TAG);
                    LOGGER.info("Received explanation in com thread #{}", threadIndex);
                    result.add(elementResult);
                }
            }));
        }

        threads.forEach(Thread::start);
        threads.forEach(t -> {
            try {
                t.join();
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        });

        LOGGER.info("Collected {} explanations", result.size());
        @SuppressWarnings("unchecked")
        AnchorResult<T>[] resultArray = result.toArray(new AnchorResult[0]);
        return resultArray;
    }

    @Override
    protected void init() {
        while (!isIsFinalizeCalled()) {
            FastMPJUtil.signalToRoot(READY_TAG);
            LOGGER.info("Slave #{} signalling ready", me);

            final T element = FastMPJUtil.receive(0, SEND_ELEMENT_TAG);
            LOGGER.info("Slave #{} received element {}", me, element);
            AnchorResult<T> result = AnchorConstructionBuilder.buildForSP(builder, element).constructAnchor(false);

            LOGGER.info("Slave #{} responding explanation {}", me, result);
            FastMPJUtil.send(result, 0, RECEIVE_ELEMENT_TAG);
        }
    }

}
