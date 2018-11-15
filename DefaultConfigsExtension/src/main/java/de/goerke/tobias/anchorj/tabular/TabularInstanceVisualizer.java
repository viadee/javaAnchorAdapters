package de.goerke.tobias.anchorj.tabular;

import java.math.RoundingMode;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import de.goerke.tobias.anchorj.AnchorCandidate;
import de.goerke.tobias.anchorj.AnchorResult;

/**
 * May be used to visualize an instance of the algorithms result for the user.
 */
public class TabularInstanceVisualizer {
    private final Map<TabularFeature, Map<Object, FeatureValueMapping>> featureValueMapping;

    /**
     * Constructs the instance.
     *
     * @param mappings the mappings used for transforming values
     */
    public TabularInstanceVisualizer(Map<TabularFeature, Map<Object, FeatureValueMapping>> mappings) {
        this.featureValueMapping = mappings;
    }

    private static AnchorCandidate getCandidateForFeatureNr(AnchorResult<?> result, Integer featureNr) {
        AnchorCandidate current = result;
        while (current != null) {
            if (current.getAddedFeature().equals(featureNr))
                return current;
            current = current.getParentCandidate();
        }
        throw new IllegalStateException("Illegal result hierarchy");
    }

    private static <K, V> Map<V, K> invertMap(Map<K, V> toInvert) {
        final Map<V, K> result = new LinkedHashMap<>();
        for (Map.Entry<K, V> entry : toInvert.entrySet()) {
            result.put(entry.getValue(), entry.getKey());
        }
        return result;
    }

    /**
     * Formats the result readable to the user.
     *
     * @param anchorResult the algorithm's result
     * @return a String visualizing the result
     */
    public String visualizeResult(AnchorResult<TabularInstance> anchorResult) {
        final String[] text = instanceToText(anchorResult.getInstance());
        final String[] explanation = new String[anchorResult.getOrderedFeatures().size()];
        int index = 0;
        final DecimalFormat df = new DecimalFormat("#.##");
        df.setRoundingMode(RoundingMode.CEILING);
        df.setDecimalFormatSymbols(DecimalFormatSymbols.getInstance(Locale.US));
        for (Integer featureNr : anchorResult.getOrderedFeatures()) {
            AnchorCandidate candidate = getCandidateForFeatureNr(anchorResult, featureNr);
            explanation[index++] = text[featureNr] + " [" + df.format(candidate.getAddedPrecision()) + ","
                    + df.format(candidate.getAddedCoverage()) + "]";
        }
        // TODO refactor?!
        return "IF ( " + String.join(" AND " + System.lineSeparator(), explanation) + ")"
                + System.lineSeparator() + "THEN PREDICT " +
                getLabelMapping().getOrDefault(anchorResult.getLabel(),
                        new CategoricalValueMapping(null, anchorResult.getLabel(), anchorResult.getLabel()));
    }

    public String[] getAnchorAsPredicateList(AnchorResult<TabularInstance> anchorResult) {
        final String[] text = instanceToText(anchorResult.getInstance());
        final String[] explanation = new String[anchorResult.getOrderedFeatures().size()];
        int index = 0;

        for (Integer featureNr : anchorResult.getOrderedFeatures()) {
            explanation[index++] = text[featureNr];
        }

        return explanation;
    }

    /**
     * Formats an instance readable to the user.
     *
     * @param explainedInstance      the explained instance
     * @param explainedInstanceLabel the explained instance's label
     * @return a String visualizing the result
     */
    public String visualizeInstance(TabularInstance explainedInstance, int explainedInstanceLabel) {
        return String.join(" AND ", instanceToText(explainedInstance)) +
                " with label " + getLabelMapping().get(explainedInstanceLabel);
    }

    private String[] instanceToText(TabularInstance explainedInstance) {
        final List<String> result = new ArrayList<>();
        Map<Integer, FeatureValueMapping> instanceValueMapping = this.getInstanceValueMapping(explainedInstance);
        instanceValueMapping.keySet().stream().sorted(Integer::compareTo).forEachOrdered((arrayIndex) -> {
            FeatureValueMapping value = instanceValueMapping.get(arrayIndex);
            String tmp;
            switch (value.getFeature().getColumnType()) {
                case CATEGORICAL:
                    tmp = ((CategoricalValueMapping) value).getCategoricalValue().toString();
                    break;
                case NATIVE:
                    tmp = ((NativeValueMapping) value).getValue().toString();
                    break;
                case NOMINAL:
                    MetricValueMapping metric = (MetricValueMapping) value;
                    tmp = "Range(" + metric.getMinValue() + ", " + metric.getMaxValue() + ")";
                    break;
                default:
                    throw new IllegalArgumentException("column type " +
                            value.getFeature().getColumnType() + " not handled");
            }
            result.add(value.getFeature().getName() + " = " + tmp);
        });

        return result.toArray(new String[0]);
    }

    private Map<Integer, FeatureValueMapping> getInstanceValueMapping(TabularInstance explainedInstance) {
        Map<Integer, FeatureValueMapping> featureValues = new HashMap<>(explainedInstance.getFeatureCount());
        for (Map.Entry<TabularFeature, Map<Object, FeatureValueMapping>> entry : featureValueMapping.entrySet()) {
            final String featureName = entry.getKey().getName();
            Integer featureArrayIndex = explainedInstance.getFeatureArrayIndex(featureName);

            final Object instanceValue = explainedInstance.getValue(featureName);
            FeatureValueMapping value = entry.getValue().getOrDefault(instanceValue,
                    new NativeValueMapping(entry.getKey(), instanceValue));
            featureValues.put(featureArrayIndex, value);
        }

        return featureValues;
    }

    /**
     * @param anchorResult
     * @return mapping of the anchor. !IMPORTANT! the key is the order of importance of the anchor.
     */
    public Map<Integer, FeatureValueMapping> getAnchor(AnchorResult<TabularInstance> anchorResult) {
        Map<Integer, FeatureValueMapping> instanceValues = this.getInstanceValueMapping(anchorResult.getInstance());

        Map<Integer, FeatureValueMapping> result = new HashMap<>();
        for (Integer arrayIndex : anchorResult.getOrderedFeatures()) {
            result.put(arrayIndex, instanceValues.get(arrayIndex));
        }

        return result;
    }

    private Map<Object, FeatureValueMapping> getLabelMapping() {
        return featureValueMapping.entrySet().stream().filter(e -> e.getKey().isTargetFeature()).map(Map.Entry::getValue)
                .findFirst().orElse(Collections.emptyMap());
    }

}
