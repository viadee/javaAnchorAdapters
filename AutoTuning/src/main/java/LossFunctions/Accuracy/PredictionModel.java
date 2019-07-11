package LossFunctions.Accuracy;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorResult;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Function;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PredictionModel {

    private static final Logger LOGGER = LoggerFactory.getLogger(PredictionModel.class);

    /**
     * - if one rule applies predict the label according to rule
     * - if two rules apply predict according to more precise rule
     * - if both rules are equally precise predict randomly
     */

    private List<Rule> rules;

    public PredictionModel(List<AnchorResult<TabularInstance>> globalExplanations) {
        this.train(globalExplanations);
    }

    /**
     * @param globalExplanations
     */
    public void train(List<AnchorResult<TabularInstance>> globalExplanations) {

        List<Rule> rules = new ArrayList<>();

        for (AnchorResult<TabularInstance> anchor : globalExplanations) {
            rules.add(new Rule(anchor));
        }

        // sort by precision so higher precisions get prioritized
        rules.sort(Comparator.comparingDouble(Rule::getPrecision).reversed());

        this.rules = rules;
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

        for (Rule r : this.rules) {
            int numberMatches = 0;

            ruleNumber++;
            System.out.println("Check rule " + ruleNumber + " with precision " + r.getPrecision() + " and label " + r.getLabel());

            for (Feature f : r.getFeature()) {

                if (f.getClass().equals(CategoricalFeature.class)) {

                    System.out.println("Feature: " + f.getName() + " - Instance value: " + " " + instance.getTransformedValue(f.getName()) + " ---- Rule value: " + ((CategoricalFeature) f).getValue());

                    if (instance.getTransformedValue(f.getName()) == ((CategoricalFeature) f).getValue()) {
                        numberMatches++;
                    }
                } else if (f.getClass().equals(MetricFeature.class)) {
                    System.out.println("Feature: " + f.getName() +" - Instance value: " + instance.getTransformedValue(f.getName()) + " ---- Rule value: " + ((MetricFeature) f).getLowerBound() + " " + ((MetricFeature) f).getUpperBound());
                    double instanceValue = ((Number)instance.getTransformedValue(f.getName())).doubleValue();

                    if (instanceValue >= ((MetricFeature) f).getLowerBound() && instanceValue <= ((MetricFeature) f).getUpperBound()) {
                        numberMatches++;
                    }
                }
            }

            if (numberMatches == (r.getFeature().size())) {
                LOGGER.info("Predict " + r.getLabel() + " for the instance based on rule " + ruleNumber + ".");
//                System.out.println("Predict " + r.getLabel() + " for the instance based on rule " + ruleNumber + ".");
                return r.getLabel();
            }
        }

        LOGGER.info("No rule found to predict the instance.");
        return -1;
    }
    
}
