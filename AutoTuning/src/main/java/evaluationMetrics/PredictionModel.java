package evaluationMetrics;

import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.DataInstance;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * The abstract class for creating a prediction model based on a given set of rules by Anchors. Applies rules as
 * following:
 *
 * - if one rule applies predict the label according to rule
 * - if two rules apply predict according to more precise rule
 * - if both rules are equally precise predict randomly
 * - if no rule applies do not predict
 */
public abstract class PredictionModel<T extends DataInstance<?>> {


    protected final List<AnchorResult<T>> rules;

    /**
     * taking a set of rules, shuffling the rules and then sorting them by precision to find the most precise rule
     * earlier
     *
     * @param rules
     */
    public PredictionModel(List<AnchorResult<T>> rules) {

        Collections.shuffle(rules);
        // sort by precision so higher precisions get prioritized
        rules.sort(Comparator.comparingDouble(AnchorResult<T>::getPrecision).reversed());
        this.rules = rules;
    }

    public abstract List<Integer> predict(T[] instances);

    public abstract int predictSingle(T instance);

}
