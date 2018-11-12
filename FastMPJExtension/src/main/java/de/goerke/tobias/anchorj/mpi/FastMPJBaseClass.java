package de.goerke.tobias.anchorj.mpi;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Base class for all FastMPJ explainers
 */
abstract class FastMPJBaseClass {
    private static final Logger LOGGER = LoggerFactory.getLogger(FastMPJBaseClass.class);

    private static boolean isFinalizeCalled = false;
    protected final int nProcesses;
    protected final int me;
    private boolean isInitialized = false;

    /**
     * Creates the MPI sampling service
     *
     * @param args String array containing the arguments passed at the main's call
     */
    public FastMPJBaseClass(final String[] args) {
        this(FastMPJInitializer.initEnvironment(args));
    }

    /**
     * Creates the MPI sampling service
     *
     * @param initializerResult result of a self-initialized environment
     */
    public FastMPJBaseClass(int[] initializerResult) {
        super();
        this.nProcesses = initializerResult[0];
        this.me = initializerResult[1];
        if (this.me == 0)
            this.isInitialized = true;
    }

    /**
     * @return true, if close has been called
     */
    public static boolean isIsFinalizeCalled() {
        return isFinalizeCalled;
    }

    /**
     * Specific, internally used initialize method
     */
    protected abstract void init();

    /**
     * This method initializes the service, i.e. "traps" all worker processes in an endless loop, listening to the
     * master process.
     * <p>
     * Needs to be called for all slave processes.
     * <p>
     * This method does not need to be called for the master process
     */
    public void initialize() {
        isInitialized = true;
        if (me > 0) {
            init();
            close();
        }
    }

    /**
     * Finalizes the FastMPJ environment. Needs to be called to kill all processes at the end of the application.
     */
    public void close() {
        LOGGER.info("Finalizing environment");
        isFinalizeCalled = true;
        FastMPJInitializer.finalizeEnvironment();
    }

    /**
     * @return the current worker index
     */
    public int getMe() {
        return me;
    }

    /**
     * @return the total number of processes
     */
    public int getnProcesses() {
        return nProcesses;
    }

    /**
     * @return true, if instance has been initialized
     */
    public boolean isInitialized() {
        return isInitialized;
    }
}
