package de.viadee.xai.anchor.adapter.tabular;

import de.viadee.xai.anchor.adapter.tabular.column.GenericColumn;
import de.viadee.xai.anchor.adapter.tabular.util.FormatTools;
import de.viadee.xai.anchor.algorithm.AnchorCandidate;
import de.viadee.xai.anchor.algorithm.AnchorResult;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * May be used to visualize an instance of the algorithms result for the user.
 */
public class TabularInstanceVisualizer {

    private static AnchorCandidate getCandidateForFeatureNr(AnchorResult<?> result, Integer featureNr) {
        AnchorCandidate current = result;
        while (current != null) {
            if (current.getAddedFeature().equals(featureNr))
                return current;
            current = current.getParentCandidate();
        }
        throw new IllegalStateException("Illegal result hierarchy");
    }

    /**
     * Visualizes an instance by describing its feature values.
     * <p>
     * Displays the non-discretized values
     *
     * @param instance the instance to describe
     * @return the result, one string for each feature
     */
    public String visualizeInstance(TabularInstance instance) {
        final List<String> result = new ArrayList<>();
        for (int i = 0; i < instance.getFeatures().length; i++) {
            final GenericColumn column = instance.getFeatures()[i];
            final Serializable transformedValue = instance.getTransformedValue(column);

            result.add(column.getName() + "='" + transformedValue.toString() + "'");
        }
        if (instance.getTargetFeature() != null && instance.getTransformedLabel() != null) {
            result.add("WITH LABEL " + instance.getTargetFeature().getName() + "='" + instance.getTransformedLabel().toString() + "'");
        }

        return String.join(System.lineSeparator(), result.toArray(new String[0]));
    }

    /**
     * Visualizes a result by describing its feature values
     *
     * @param anchorResult the result to describe
     * @return the result, one string for each feature
     */
    public String visualizeResult(AnchorResult<TabularInstance> anchorResult) {
        final List<String> featureText = new ArrayList<>();
        final TabularInstance instance = anchorResult.getInstance();
        for (final Integer featureNr : anchorResult.getOrderedFeatures()) {
            final GenericColumn feature = instance.getFeatures()[featureNr];
            final AnchorCandidate candidate = getCandidateForFeatureNr(anchorResult, featureNr);
            featureText.add(feature.getName() + " " + feature.getDiscretizer()
                    .getTransition(instance.getValue(featureNr)).getDiscretizationOrigin().outputFormat()
                    + " {" +
                    FormatTools.roundToTwo(candidate.getAddedPrecision()) + ", " +
                    FormatTools.roundToTwo(candidate.getAddedCoverage()) +
                    "}");
        }

        return "IF " + String.join(" AND " + System.lineSeparator(), featureText.toArray(new String[0])) +
                System.lineSeparator() +
                "THEN PREDICT " +
                instance.getTransformedLabel() + " (" + FormatTools.roundToTwo(instance.getDiscretizedLabel()) + ")" +
                System.lineSeparator() +
                "WITH PRECISION " + FormatTools.roundToTwo(anchorResult.getPrecision()) +
                " AND COVERAGE " + FormatTools.roundToTwo(anchorResult.getCoverage());
    }

    /**
     * Visualizes a collection of results by describing its feature values
     *
     * @param anchorResults the results to describe
     * @return the result, one string for each feature
     */
    public String visualizeGlobalResults(List<AnchorResult<TabularInstance>> anchorResults) {
        final List<String> result = new ArrayList<>();
        for (int i = 0; i < anchorResults.size(); i++) {
            String[] current = new String[3];
            current[0] = "===Global Result #" + (i + 1) + "===";
            current[1] = visualizeResult(anchorResults.get(i));
            current[2] = "HAVING EXCLUSIVE COVERAGE OF ";
            // TODO value missing. What is this about any way?
            result.add(String.join(System.lineSeparator(), current));
        }
        return String.join(System.lineSeparator(), result);
    }

}
