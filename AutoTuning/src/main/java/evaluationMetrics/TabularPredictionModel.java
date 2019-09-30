package evaluationMetrics;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;
import de.viadee.xai.anchor.algorithm.AnchorResult;

import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The prediction model for Tabular Data. Predicts the label given by a ruleset for each instance in a tabular
 * dataset.
 */
public class TabularPredictionModel extends PredictionModel<TabularInstance> {

    private static final Logger LOGGER = LoggerFactory.getLogger(TabularPredictionModel.class);

    public TabularPredictionModel(List<AnchorResult<TabularInstance>> globalExplanations) {
        super(globalExplanations);
    }

    /**
     * Get a list of predicted discretized class labels
     *
     * @param instances the instances of the dataset
     * @return a list of predictions
     */
    public List<Integer> predict(TabularInstance[] instances) {

        List<Integer> predictions = new ArrayList<>();

        for (TabularInstance instance : instances) {
            predictions.add(this.predictSingle(instance));
        }
        return predictions;
    }

    /**
     * Get the predicted discretized label for a single instance
     *
     * @param instance the instance to be predicted
     * @return the predicted label
     */
    public int predictSingle(TabularInstance instance) {

        int ruleNumber = 0;

        for (AnchorResult<TabularInstance> rule : this.rules) {
            ruleNumber++;

            if (checkIfRuleApplies(instance, rule)) {
                LOGGER.info("Predict " + rule.getExplainedInstanceLabel() + " for the instance based on rule " + ruleNumber + ".");
                return rule.getExplainedInstanceLabel();
            }
        }

        LOGGER.info("No rule found to predict the instance.");
        return -1;
    }

    /**
     * Check if rule applies to tabular instance
     *
     * @param instance tabular instance
     * @param rule applied rule as tabular instance
     * @return bool whether the rule applies or not
     */
    private boolean checkIfRuleApplies(TabularInstance instance, AnchorResult<TabularInstance> rule) {
        for (int f : rule.getCanonicalFeatures()) {

            double instanceValue = instance.getValue(f);
            double ruleValue = rule.getInstance().getValue(f);

            if (instanceValue != ruleValue) {
                return false;
            }
        }
        return true;
    }
    
}
