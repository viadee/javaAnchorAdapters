package de.viadee.xai.anchor.adapter.rmodule;

import de.viadee.xai.anchor.algorithm.coverage.CoverageIdentification;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Represents a remote {@link CoverageIdentification} function
 */
class RemoteCallbackCoverageIdentification implements CoverageIdentification {
    private final BufferedReader in;
    private final PrintWriter out;

    /**
     * Constructs the instance
     *
     * @param in  the reader used to receive messages
     * @param out the writer used to send messages
     */
    RemoteCallbackCoverageIdentification(final BufferedReader in, final PrintWriter out) {
        super();
        this.in = in;
        this.out = out;
    }


    @Override
    public double calculateCoverage(final Set<Integer> featureSet) {
        final JSONObject coverage = new JSONObject();
        coverage.append("status", "coverage_request");
        coverage.append("features", featureSet.stream()
                .map(i -> i + 1)
                .collect(Collectors.toSet()));
        out.println(coverage.toString());

        try {
            final String input = in.readLine();
            if (input == null)
                throw new RuntimeException("Empty response received");
            final JSONObject obj = new JSONObject(input);
            return obj.getDouble("coverage");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
