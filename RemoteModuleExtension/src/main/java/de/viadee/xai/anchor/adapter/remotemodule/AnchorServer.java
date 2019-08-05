package de.viadee.xai.anchor.adapter.remotemodule;

import de.viadee.xai.anchor.algorithm.AnchorConstructionBuilder;
import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.exploration.KL_LUCB;
import org.apache.commons.cli.*;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

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
            final Map<String, Option> optionsMap = new HashMap<>();

            optionsMap.put("port", new Option("p", "port", true,
                    "The socket connection port used to listen to incoming data"));
            optionsMap.put("timeout", new Option("to", "timeout", true, "The number of seconds " +
                    "the server will listen after each last communications act. Set 0 to disable automatic shutdown. " +
                    "Default = 60 seconds. "));
            optionsMap.put("maxAnchorSize", new Option("maxAnchor", "maxAnchorSize", true,
                    "Max combined features of the resulting anchor."));
            optionsMap.put("beamSize", new Option("beam", "beamSize", true,
                    "Parameter B: size of the current candidates for beam search."));
            optionsMap.put("delta", new Option("delta", "delta", true,
                    "The delta value describing the probability of identifying the best arm confidence."));
            optionsMap.put("epsilon", new Option("epsilon", "epsilon", true,
                    "The maximum tolerated error == tolerance."));
            optionsMap.put("tau", new Option("tau", "tau", true,
                    "The desired precision an anchor needs to achieve. If no candidate achieves at least " +
                            "this precision, the one with the best precision will be returned"));
            optionsMap.put("tauDiscrepancy", new Option("tauD", "tauDiscrepancy", true,
                    "Usually, it is practically infeasible to sample until the mean and the upper/lower " +
                            "bounds simultaneously fall below or above the tau threshold. Therefore, this variable " +
                            "may be introduced to control this discrepancy."));
            optionsMap.put("initSampleCount", new Option("initS", "initSampleCount", true,
                    "The number of evaluations sampled for each candidate before it gets evaluated by " +
                            "the best arm identification algorithm. While theoretically, a guarantee that no " +
                            "candidates get discarded due to too few samples is provided by delta, using this " +
                            "argument has practical advantages."));
            optionsMap.put("allowSuboptimalSteps", new Option("subSteps", "allowSuboptimalSteps", true,
                    "If set to false, candidates that are returned by the best arm identification get " +
                            "removed when their precision is lower than their parent's"));
            optionsMap.put("batchSize", new Option("batch", "batchSize", true,
                    "The number of evaluations to perform on each pull of the arm."));

            optionsMap.get("port").setRequired(true);

            final Options options = new Options();
            optionsMap.values().forEach(options::addOption);

            final CommandLineParser parser = new DefaultParser();
            final HelpFormatter formatter = new HelpFormatter();
            CommandLine cmd;

            try {
                cmd = parser.parse(options, args);
            } catch (ParseException e) {
                formatter.printHelp("utility-name", options);
                throw new RuntimeException(e);
            }

            final Map<String, Object> optionValueMap = new HashMap<>();
            try {
                putValue(cmd, optionsMap, optionValueMap, "port", null, 'i');
                putValue(cmd, optionsMap, optionValueMap, "timeout", "60", 'i');
                putValue(cmd, optionsMap, optionValueMap, "maxAnchorSize", "0", 'i');
                putValue(cmd, optionsMap, optionValueMap, "beamSize", "2", 'i');
                putValue(cmd, optionsMap, optionValueMap, "delta", "0.9", 'd');
                putValue(cmd, optionsMap, optionValueMap, "epsilon", "0.05", 'd');
                putValue(cmd, optionsMap, optionValueMap, "tau", "0.9", 'd');
                putValue(cmd, optionsMap, optionValueMap, "tauDiscrepancy", "0.05", 'd');
                putValue(cmd, optionsMap, optionValueMap, "initSampleCount", "20", 'i');
                putValue(cmd, optionsMap, optionValueMap, "allowSuboptimalSteps", "true", 'b');
                putValue(cmd, optionsMap, optionValueMap, "batchSize", "100", 'i');
            } catch (NumberFormatException e) {
                throw new RuntimeException("Invalid option value", e);
            }

            System.out.println(
                    "Starting Server with following params:" + System.lineSeparator() +
                            optionValueMap.entrySet().stream()
                                    .map(e -> "\t" + e.getKey() + ": " + e.getValue())
                                    .collect(Collectors.joining(System.lineSeparator())));

            try {
                try (final ServerSocket listener = new ServerSocket((int) optionValueMap.get("port"))) {
                    if (((int) optionValueMap.get("timeout")) > 0)
                        listener.setSoTimeout((int) optionValueMap.get("timeout") * 1000);
                    // TODO this is dangerous. Concurrent processes will answer to same algorithm thread.
                    //  evaluate ID and relay to correct instance before enabling threading!!
                    //  Furthermore, it is unclear whether threading using serversockets on a local instance would work
                    //  anyway
                    //final ExecutorService pool = Executors.newFixedThreadPool(1);
                    while (true) {
                        final Socket socket = listener.accept();
                        new AnchorService(
                                socket,
                                (int) optionValueMap.get("maxAnchorSize"),
                                (int) optionValueMap.get("beamSize"),
                                (double) optionValueMap.get("delta"),
                                (double) optionValueMap.get("epsilon"),
                                (double) optionValueMap.get("tau"),
                                (double) optionValueMap.get("tauDiscrepancy"),
                                (int) optionValueMap.get("initSampleCount"),
                                (boolean) optionValueMap.get("allowSuboptimalSteps"),
                                (int) optionValueMap.get("batchSize"))
                                .run();
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

    private static void putValue(CommandLine cmd, Map<String, Option> optionsMap, Map<String, Object> resultMap,
                                 String optionKey, String defaultValue, char type) {
        final Option option = optionsMap.get(optionKey);
        final String optionValue = (defaultValue != null)
                ? cmd.getOptionValue(option.getOpt(), defaultValue)
                : cmd.getOptionValue(optionsMap.get(optionKey).getOpt());

        Object targetValue;
        switch (type) {
            case 'i':
                targetValue = Integer.parseInt(optionValue);
                break;
            case 'd':
                targetValue = Double.parseDouble(optionValue);
                break;
            case 'b':
                targetValue = Boolean.parseBoolean(optionValue);
                break;
            default:
                throw new RuntimeException("Unexpected type");
        }
        resultMap.put(option.getLongOpt(), targetValue);
    }

    private static class AnchorService implements Runnable {
        private final Socket socket;

        private final int maxAnchorSize;
        private final int beamSize;
        private final double delta;
        private final double epsilon;
        private final double tau;
        private final double tauDiscrepancy;
        private final int initSampleCount;
        private final boolean allowSuboptimalSteps;
        private final int batchSize;

        private AnchorService(Socket socket, int maxAnchorSize, int beamSize, double delta, double epsilon, double tau,
                              double tauDiscrepancy, int initSampleCount, boolean allowSuboptimalSteps, int batchSize) {
            this.socket = socket;
            this.maxAnchorSize = maxAnchorSize;
            this.beamSize = beamSize;
            this.delta = delta;
            this.epsilon = epsilon;
            this.tau = tau;
            this.tauDiscrepancy = tauDiscrepancy;
            this.initSampleCount = initSampleCount;
            this.allowSuboptimalSteps = allowSuboptimalSteps;
            this.batchSize = batchSize;
        }

        @Override
        public void run() {
            System.out.println("Connected: " + socket);
            boolean quit = false;
            try {
                final BufferedReader in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                final PrintWriter out = new PrintWriter(socket.getOutputStream(), true);

                while (true) {
                    System.out.println("Starting listening");
                    final String input = in.readLine();
                    System.out.println("Got input: " + input);
                    if (input == null)
                        throw new RuntimeException("Empty response received");
                    try {
                        final JSONObject obj = new JSONObject(input);

                        if (obj.has("quit") && obj.getInt("quit") == 1) {
                            quit = true;
                            return;
                        }

                        final String id = obj.getString("id");
                        final int dimension = obj.getInt("instance");
                        final RemoteInstance instance = new RemoteInstance(dimension);

                        final RemoteCallbackSamplingFunction remoteCallbackSamplingFunction =
                                new RemoteCallbackSamplingFunction(in, out);
                        final RemoteCallbackCoverageIdentification coverageIdentification =
                                new RemoteCallbackCoverageIdentification(in, out);

                        final AnchorConstructionBuilder<RemoteInstance> remoteInstanceAnchorConstructionBuilder =
                                new AnchorConstructionBuilder<>(
                                        remoteCallbackSamplingFunction, instance, 0)
                                        .setCoverageIdentification(coverageIdentification)
                                        .setBeamSize(beamSize)
                                        .setDelta(delta)
                                        .setEpsilon(epsilon)
                                        .setTau(tau)
                                        .setTauDiscrepancy(tauDiscrepancy)
                                        .setInitSampleCount(initSampleCount)
                                        .setAllowSuboptimalSteps(allowSuboptimalSteps)
                                        .setBestAnchorIdentification(new KL_LUCB(batchSize));
                        if (maxAnchorSize > 0)
                            remoteInstanceAnchorConstructionBuilder.setMaxAnchorSize(maxAnchorSize);

                        final AnchorResult anchorResult = remoteInstanceAnchorConstructionBuilder
                                .build().constructAnchor();
                        //send response
                        final JSONObject response = new JSONObject();
                        response.append("id", id);
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
