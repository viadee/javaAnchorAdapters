
public class RSamplingFunction extends DefaultSamplingFunction{

    @Override
    public double evaluate(AnchorCandidate candidate, int samplesToEvaluate, int explainedInstanceLabel) {
        if (samplesToEvaluate < 1)
            return 0;
/** RRRRRRRRRR
 *         final PerturbationFunction.PerturbationResult<T> perturbationResult = perturbationFunction.perturb(
                candidate.getCanonicalFeatures(), samplesToEvaluate);
        final int[] predictions = classificationFunction.predict(perturbationResult.getRawResult());

        final int matchingLabels = Math.toIntExact(IntStream.of(predictions)
                .filter(p -> p == explainedInstanceLabel).count());
*/
        matchingLabels = R.callback(candidate, samplesToEvaluate,explainedInstanceLabel);


        candidate.registerSamples(samplesToEvaluate, matchingLabels);

        double precision = matchingLabels / (double) predictions.length;
        LOGGER.trace("Sampling {} perturbations of {} has resulted in {} correct predictions, thus a precision of {}",
                samplesToEvaluate, candidate.getCanonicalFeatures(), matchingLabels, precision);
        return precision;
    }

}