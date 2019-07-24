package LossFunctions;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorResult;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PredictionModel {

    private static final Logger LOGGER = LoggerFactory.getLogger(PredictionModel.class);

    /**
     * - if one rule applies predict the label according to rule
     * - if two rules apply predict according to more precise rule
     * - if both rules are equally precise predict randomly
     */

    private final List<AnchorResult<TabularInstance>> rules;

    public PredictionModel(List<AnchorResult<TabularInstance>> globalExplanations) {
        this.rules = globalExplanations;
        // sort by precision so higher precisions get prioritized
        rules.sort(Comparator.comparingDouble(AnchorResult<TabularInstance>::getPrecision).reversed());

    }

    /**
     * @param instances
     * @return
     */
    public List<Integer> predict(TabularInstance[] instances) {

        List<Integer> predictions = new ArrayList<>();

        for (TabularInstance instance : instances) {
            predictions.add(this.predictSingle(instance));
        }
        return predictions;
    }

    /**
     * @param instance
     * @return
     */
    public int predictSingle(TabularInstance instance) {

        int ruleNumber = 0;

        for (AnchorResult<TabularInstance> rule : this.rules) {
            int numberMatches = 0;
            ruleNumber++;
//            System.out.println("Check rule " + ruleNumber + " with precision " + r.getPrecision() + " and label " + r.getLabel());

            for (int f : rule.getCanonicalFeatures()) {

                double instanceValue = instance.getValue(f);
                double ruleValue = rule.getInstance().getValue(f);

//                System.out.println("Feature: " + rule.getInstance().getFeatures()[f].getName() + " - Instance value: " + instanceValue + " ---- Rule value: " + ruleValue);

                if (instanceValue == ruleValue) {
                    numberMatches++;
                }
            }

            if (numberMatches == (rule.getCanonicalFeatures().size())) {
                LOGGER.info("Predict " + rule.getLabel() + " for the instance based on rule " + ruleNumber + ".");
                return rule.getLabel();
            }
        }

        LOGGER.info("No rule found to predict the instance.");
        return -1;
    }
    
}
