package de.viadee.xai.anchor.adapter.rmodule;

import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import org.apache.commons.cli.*;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Contains code that enables third parties to access the Java implementation by using inter-process communication
 */
public class AnchorServer {

    /**
     * Starts the anchors server
     *
     * @param args arguments
     * @throws IOException in case the Socket cannot be opened
     */
    public static void main(String[] args) throws IOException {
        try {
            final Options options = new Options();

            final Option portOption = new Option("p", "port", true, "The socket connection port " +
                    "used to listen to incoming data");
            portOption.setRequired(true);

            final Option timeoutOption = new Option("to", "timeout", true, "The number of seconds " +
                    "the server will listen after each last communications act. Set 0 to disable automatic shutdown. " +
                    "Default = 60 seconds. ");

            Stream.of(portOption, timeoutOption).forEach(options::addOption);

            final CommandLineParser parser = new DefaultParser();
            final HelpFormatter formatter = new HelpFormatter();
            CommandLine cmd;

            try {
                cmd = parser.parse(options, args);
            } catch (ParseException e) {
                formatter.printHelp("utility-name", options);
                throw new RuntimeException(e);
            }

            int portNr = 6666;
            int timeout = 60;
            try {
                portNr = Integer.parseInt(cmd.getOptionValue(portOption.getOpt()));
                timeout = Integer.parseInt(cmd.getOptionValue(timeoutOption.getLongOpt(), "60"));
            } catch (NumberFormatException e) {
                throw new RuntimeException("Invalid option value", e);
            }

            final Map<Option, Object> optionMap = new HashMap<>();
            optionMap.put(portOption, portNr);
            optionMap.put(timeoutOption, timeout);

            System.out.println(
                    "Starting Server with following params:" + System.lineSeparator() +
                            optionMap.entrySet().stream()
                                    .map(e -> "\t" + e.getKey().getLongOpt() + ": " + e.getValue())
                                    .collect(Collectors.joining(System.lineSeparator())));

            try {
                try (final ServerSocket listener = new ServerSocket(portNr)) {
                    if (timeout > 0)
                        listener.setSoTimeout(timeout * 1000);
                    // TODO this is dangerous. Concurrent processes will answer to same algorithm thread.
                    //  evaluate ID and relay to correct instance before enabling threading!!
                    //  Furthermore, it is unclear whether threading using serversockets on a local instance would work
                    //  anyway
                    //final ExecutorService pool = Executors.newFixedThreadPool(1);
                    while (true) {
                        new AnchorService(listener.accept());
                    }
                }
            } catch (SocketTimeoutException e) {
                throw new RuntimeException("The connection has timed out. Stopping listening processes.");
            }
        } catch (Throwable e) {
            System.out.println("Shutting down server due to exception: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }

    private static class AnchorService implements Runnable {
        private final Socket socket;

        private AnchorService(final Socket socket) {
            this.socket = socket;
        }

        @Override
        public void run() {
            System.out.println("Connected: " + socket);
            boolean quit = false;
            try {
                final BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                final PrintWriter out = new PrintWriter(socket.getOutputStream(), true);

                while (true) {
                    final String input = in.readLine();
                    if (input == null)
                        throw new RuntimeException("Empty response received");
                    try {
                        final JSONObject obj = new JSONObject(input);

                        if (obj.has("quit") && obj.getInt("quit") == 1) {
                            quit = true;
                            return;
                        }

                        final String id = obj.getString("id");
                        final int count = obj.getInt("count");
                        final int dimension = obj.getInt("instance");
                        final RemoteInstance instance = new RemoteInstance(dimension);

                        final RemoteCallbackSamplingFunction remoteCallbackSamplingFunction =
                                new RemoteCallbackSamplingFunction(in, out);
                        final RemoteCallbackCoverageIdentification coverageIdentification =
                                new RemoteCallbackCoverageIdentification(in, out);

                        final AnchorResult anchorResult = new AnchorConstructionBuilder<>(
                                remoteCallbackSamplingFunction, instance, 0)
                                .setCoverageIdentification(coverageIdentification)
                                .setLazyCoverageEvaluation(true)
                                .setTau(0.95)
                                .setTauDiscrepancy(0.1)
                                .build().constructAnchor();

                        //send response
                        final JSONObject response = new JSONObject();
                        response.append("id", id);
                        response.append("count", count + 1);
                        response.append("status", "response");
                        response.append("anchorResult", new JSONObject(anchorResult));
                        out.println(response.toString());
                    } catch (JSONException e) {
                        throw new RuntimeException("Could not deserialize response");
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }
            } catch (Exception e) {
                System.out.println("Error in AnchorService#run: " + socket);
                throw new RuntimeException(e);
            } finally {
                try {
                    socket.close();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
                System.out.println("Closed: " + socket);
                if (quit)
                    throw new RuntimeException("Server exit requested by client");
            }
        }

    }

}
