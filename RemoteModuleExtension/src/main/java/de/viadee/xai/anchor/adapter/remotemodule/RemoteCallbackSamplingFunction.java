package de.viadee.xai.anchor.adapter.remotemodule;


import de.viadee.xai.anchor.algorithm.AnchorCandidate;
import de.viadee.xai.anchor.algorithm.ClassificationFunction;
import de.viadee.xai.anchor.algorithm.coverage.PerturbationBasedCoverageIdentification;
import de.viadee.xai.anchor.algorithm.execution.sampling.SamplingFunction;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.stream.Collectors;


/**
 * Represents a classification function whose results are transmitted via external communication means
 */
class RemoteCallbackSamplingFunction implements SamplingFunction<RemoteInstance> {
    private final BufferedReader in;
    private final PrintWriter out;

    /**
     * Constructs the instance
     *
     * @param in  the reader used to receive messages
     * @param out the writer used to send messages
     */
    RemoteCallbackSamplingFunction(final BufferedReader in, final PrintWriter out) {
        super();
        this.in = in;
        this.out = out;
    }

    @Override
    public double evaluate(final AnchorCandidate candidate,
                           final int samplesToEvaluate,
                           final int explainedInstanceLabel) {
        if (samplesToEvaluate < 1) {
            return 0;
        }

        // Send evaluation request to client
        final JSONObject evaluate = new JSONObject();
        evaluate.append("status", "eval_request");
        evaluate.append("anchors", candidate.getCanonicalFeatures().stream()
                .map(i -> i + 1)
                .collect(Collectors.toSet()));
        evaluate.append("samplesToEvaluate", samplesToEvaluate);
        out.println(evaluate.toString());

        try {
            final String input = in.readLine();
            if (input == null) {
                throw new RuntimeException("Received response was empty");
            }
            final JSONObject obj = new JSONObject(input);
            final int matchingLabels = obj.getInt("matchingLabels");
            final double precision = obj.getDouble("precision");

            candidate.registerSamples(samplesToEvaluate, matchingLabels);

            return (precision);
        } catch (IOException e) {
            throw new RuntimeException("IOException while waiting for sample function response", e);
        } catch (JSONException e) {
            throw new RuntimeException("Could not parse the sample function response", e);
        }
    }

    @Override
    public SamplingFunction<RemoteInstance> notifyOriginChange(RemoteInstance explainedInstance) throws UnsupportedOperationException {
        throw new IllegalArgumentException("Not applicable for anchorsOnR");
    }

    @Override
    public PerturbationBasedCoverageIdentification createPerturbationBasedCoverageIdentification() {
        throw new IllegalArgumentException("Not applicable for anchorsOnR");
    }

    @Override
    public ClassificationFunction<RemoteInstance> getClassificationFunction() {
        throw new IllegalArgumentException("Not applicable for anchorsOnR");
    }

}