package de.viadee.xai.anchor.adapter.mpi;

import mpi.MPI;

/**
 * Utility class providing some commonly used MPI procedures
 */
public enum FastMPJUtil {;

    /**
     * Sends (non-blocking) a boolean to the root
     *
     * @param tag the tag
     */
    public static void signalToRoot(int tag) {
        MPI.COMM_WORLD.Isend(new boolean[]{true}, 0, 1, MPI.BOOLEAN, 0, tag);
    }

    /**
     * Waits for a processes' signal
     *
     * @param i   the process number
     * @param tag the tag
     */
    public static void waitForSignal(int i, int tag) {
        MPI.COMM_WORLD.Recv(new boolean[1], 0, 1, MPI.BOOLEAN, i, tag);
    }


    /**
     * Sends (non-blocking) an object to a slaves
     *
     * @param object the object to be send
     * @param i      the slave process
     * @param tag    the tag
     */
    public static void send(Object object, int i, int tag) {
        MPI.COMM_WORLD.Isend(new Object[]{object}, 0, 1, MPI.OBJECT, i, tag);
    }

    /**
     * Received (blocking) an object from a slaves
     *
     * @param i   the process slave
     * @param tag the tag
     * @param <T> type of the received object
     * @return the received object
     */
    @SuppressWarnings("unchecked")
    public static <T> T receive(int i, int tag) {
        Object[] result = new Object[1];
        MPI.COMM_WORLD.Recv(result, 0, 1, MPI.OBJECT, i, tag);
        return (T) result[0];
    }
}
