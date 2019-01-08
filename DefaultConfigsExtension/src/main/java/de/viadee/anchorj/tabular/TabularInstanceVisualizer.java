package de.viadee.anchorj.tabular;

import java.io.Serializable;
import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import de.viadee.anchorj.AnchorCandidate;
import de.viadee.anchorj.AnchorResult;
import de.viadee.anchorj.tabular.column.GenericColumn;
import de.viadee.anchorj.tabular.discretizer.DiscretizerRelation;

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
     * Visualizes an instance by describing its feature values
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

    private String describeValue(TabularInstance instance, GenericColumn feature) {
        // TODO test change
        DiscretizerRelation relation = feature.getDiscretizer().unApply(instance.getValue(feature));
        switch (relation.getFeatureType()) {
            case METRIC:
                return " IN INCL RANGE [" + relation.getConditionMin() + "," + relation.getConditionMax() + "]";
            case CATEGORICAL:
                return " = '" + relation.getCategoricalValue() + "'";
            case UNDEFINED:
            default:
                throw new IllegalArgumentException("Feature of type " + relation.getFeatureType() + " not handled");
        }

//        final Map<Serializable, Integer> mapping = featureValueMapping.get(feature);
//        final List<Serializable> belongingValues = new ArrayList<>();
//
//        for (final Map.Entry<Serializable, Integer> entry : mapping.entrySet()) {
//            if (entry.getValue().equals(instance.getValue(feature))) {
//                belongingValues.add(entry.getKey());
//            }
//        }
//
//        if (belongingValues.isEmpty())
//            throw new IllegalArgumentException("Should not be empty");
//        if (belongingValues.size() == 1)
//            return "='" + belongingValues.get(0) + "'";
//        if (belongingValues.stream().allMatch(o -> o instanceof Number)) {
//            final List<Number> numberList = belongingValues.stream().map(o -> (Number) o)
//                    .sorted(Comparator.comparingDouble(Number::doubleValue)).collect(Collectors.toList());
//            // TODO test this behavior
//            return " IN RANGE [" + numberList.get(0) + "," + numberList.get(numberList.size() - 1) + "]";
//        }
//        return " IN [" + belongingValues.stream().map(Serializable::toString).collect(Collectors.joining(",")) + "]";
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
        final DecimalFormat df = new DecimalFormat("#.##");
        df.setRoundingMode(RoundingMode.CEILING);
        df.setDecimalFormatSymbols(DecimalFormatSymbols.getInstance(Locale.US));
        for (final Integer featureNr : anchorResult.getOrderedFeatures()) {
            final GenericColumn feature = instance.getFeatures()[featureNr];
            final AnchorCandidate candidate = getCandidateForFeatureNr(anchorResult, featureNr);
            featureText.add(feature.getName() + describeValue(instance, feature)
                    + " {" + df.format(candidate.getAddedPrecision()) + ","
                    + df.format(candidate.getAddedCoverage()) + "}");
        }
        String labelText = instance.getDiscretizedLabel().toString();

        return "IF " + String.join(" AND " + System.lineSeparator(), featureText.toArray(new String[0])) +
                System.lineSeparator() +
                "THEN PREDICT " + labelText +
                System.lineSeparator();
        // TODO do something + "WITH PRECISION " + anchorResult.getPrecision() + " AND COVERAGE " + anchorResult.getCoverage();
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
            result.add(String.join(System.lineSeparator(), current));
        }
        return String.join(System.lineSeparator(), result);
    }

}
