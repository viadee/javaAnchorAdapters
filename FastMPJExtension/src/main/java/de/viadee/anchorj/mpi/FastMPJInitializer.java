package de.viadee.anchorj.mpi;

import mpi.MPI;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Used to initialize and finalize the FastMPJ framework.
 */
public enum FastMPJInitializer {;
    private static final Logger LOGGER = LoggerFactory.getLogger(FastMPJInitializer.class);

    /**
     * Initializes the parallel environment.
     *
     * @param args Program args (with FastMPJ data).
     * @return an int array containing the total count of workers in index 0 and this worker's rank in index 1
     */
    public static int[] initEnvironment(String[] args) {
        MPI.Init(args);
        int[] result = new int[2];
        result[0] = MPI.COMM_WORLD.Size();
        result[1] = MPI.COMM_WORLD.Rank();
        LOGGER.info("Successfully initialized FastMPJ environment. This is worker worker #{} of {} total workers",
                result[1], result[0]);
        return result;
    }

    /**
     * Finalizes the environment
     */
    public static void finalizeEnvironment() {
        MPI.Finalize();
    }
}
