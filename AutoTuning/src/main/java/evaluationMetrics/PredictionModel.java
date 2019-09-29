package evaluationMetrics;

import de.viadee.xai.anchor.algorithm.AnchorResult;
import de.viadee.xai.anchor.algorithm.DataInstance;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public abstract class PredictionModel<T extends DataInstance<?>> {

    /**
     * - if one rule applies predict the label according to rule
     * - if two rules apply predict according to more precise rule
     * - if both rules are equally precise predict randomly
     */

    protected final List<AnchorResult<T>> rules;

    public PredictionModel(List<AnchorResult<T>> rules) {

        Collections.shuffle(rules);
        // sort by precision so higher precisions get prioritized
        rules.sort(Comparator.comparingDouble(AnchorResult<T>::getPrecision).reversed());
        this.rules = rules;
    }

    public abstract List<Integer> predict(T[] instances);

    public abstract int predictSingle(T instance);

}
