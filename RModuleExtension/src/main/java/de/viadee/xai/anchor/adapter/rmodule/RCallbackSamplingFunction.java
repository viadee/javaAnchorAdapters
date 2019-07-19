package de.viadee.xai.anchor.adapter.rmodule;


import de.viadee.xai.anchor.algorithm.AnchorCandidate;
import de.viadee.xai.anchor.algorithm.ClassificationFunction;
import de.viadee.xai.anchor.algorithm.coverage.PerturbationBasedCoverageIdentification;
import de.viadee.xai.anchor.algorithm.execution.sampling.SamplingFunction;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.TreeSet;
import java.util.stream.Collectors;


public class RCallbackSamplingFunction implements SamplingFunction<RInstance> {

    /**
     * Creates the sampling function.
     */
    private PrintWriter out;
    private BufferedReader in;

    public RCallbackSamplingFunction(PrintWriter out, BufferedReader in) {
        super();
        this.out = out;
        this.in = in;
    }

    @Override
    public double evaluate(AnchorCandidate candidate, int samplesToEvaluate, int explainedInstanceLabel) {
        if (samplesToEvaluate < 1) {
            return 0;
        }

        JSONObject evaluate = new JSONObject();
        evaluate.append("status", "eval_request");
        evaluate.append("anchors", candidate.getCanonicalFeatures().stream().map(i -> i + 1).collect(Collectors.toSet()));
        evaluate.append("samplesToEvaluate", samplesToEvaluate);
        out.println(evaluate.toString());

        String input;
        int matchingLabels;
        double precision = 0;
        try {
            if ((input = in.readLine()) != null) {
                JSONObject obj = new JSONObject(input);
                matchingLabels = obj.getInt("matchingLabels");
                precision = obj.getDouble("precision");
                candidate.registerSamples(samplesToEvaluate, matchingLabels);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return (precision);


///** RRRRRRRRRR
// *         final PerturbationFunction.PerturbationResult<T> perturbationResult = perturbationFunction.perturb(
// candidate.getCanonicalFeatures(), samplesToEvaluate);
// final int[] predictions = classificationFunction.predict(perturbationResult.getRawResult());
//
// final int matchingLabels = Math.toIntExact(IntStream.of(predictions)
// .filter(p -> p == explainedInstanceLabel).count());
// */
//        candidate.getCanonicalFeatures();
//
//        matchingLabels = R.callback(candidate, samplesToEvaluate, explainedInstanceLabel);
//
//
//        candidate.registerSamples(samplesToEvaluate, matchingLabels);
//
//        double precision = matchingLabels / (double) predictions.length;
//        LOGGER.trace("Sampling {} perturbations of {} has resulted in {} correct predictions, thus a precision of {}",
//                samplesToEvaluate, candidate.getCanonicalFeatures(), matchingLabels, precision);
//        return precision;

    }

    @Override
    public SamplingFunction<RInstance> notifyOriginChange(RInstance explainedInstance) throws UnsupportedOperationException {
        throw new UnsupportedOperationException();
    }

    @Override
    public PerturbationBasedCoverageIdentification createPerturbationBasedCoverageIdentification() {
        throw new RuntimeException("Not yet implemented");
    }

    @Override
    public ClassificationFunction<RInstance> getClassificationFunction() {
        throw new IllegalArgumentException("Not applicable for anchorsOnR");
    }

}