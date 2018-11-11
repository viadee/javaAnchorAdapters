package de.goerke.tobias.anchorj.mpi;

import mpi.MPI;

/**
 * Utility class providing some commonly used MPI procedures
 */
public final class FastMPJUtil {

    /**
     * Sends (non-blocking) a boolean to the root
     *
     * @param tag the tag
     */
    public static void signalToRoot(int tag) {
        MPI.COMM_WORLD.Isend(new boolean[]{true}, 0, 1, MPI.BOOLEAN, 0, tag);
    }

    /**
     * Waits for all processes' signal
     *
     * @param tag   the tag
     * @param nProc number of total processes
     */
    public static void waitForAllToSignal(int tag, int nProc) {
        for (int i = 1; i < nProc; i++) {
            MPI.COMM_WORLD.Recv(new boolean[1], 0, 1, MPI.BOOLEAN, i, tag);
        }
    }

    /**
     * Sends (non-blocking) an object to all slaves
     *
     * @param object the object to be send
     * @param nProc  the number of processes
     * @param tag    the tag
     */
    public static void sendFromRootToAll(Object object, int nProc, int tag) {
        for (int i = 1; i < nProc; i++) {
            MPI.COMM_WORLD.Isend(new Object[]{object}, 0, 1, MPI.OBJECT, i, tag);
        }
    }

    /**
     * Received (blocking) an object from all slaves
     *
     * @param tag the tag
     * @param <T> type of the received object
     * @return the received object
     */
    @SuppressWarnings("unchecked")
    public static <T> T receiveFromRoot(int tag) {
        Object[] result = new Object[1];
        MPI.COMM_WORLD.Recv(result, 0, 1, MPI.OBJECT, 0, tag);
        return (T) result[0];
    }
}
