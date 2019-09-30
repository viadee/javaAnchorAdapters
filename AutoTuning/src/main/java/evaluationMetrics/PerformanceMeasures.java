package evaluationMetrics;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;

import java.util.List;
import java.util.function.Function;

/**
 * Calculates different measures based on the comparison of a list of predicted values and the predictions of
 * a classification model
 */
public class PerformanceMeasures {

    private double coverage = 0;
    private int truePositives = 0;
    private int falsePositives = 0;
    private int trueNegatives = 0;
    private int falseNegatives = 0;

    public enum Measure {
        ACCURACY,
        PRECISION,
        RECALL,
        SPECIFICITY,
        F1
    }

    public PerformanceMeasures() {
    }

    public PerformanceMeasures(List<Integer> predictedValues, Function<TabularInstance, Integer> predictFunction, TabularInstance[] data) {
        calcPerformance(predictedValues, predictFunction, data);
    }

    public double calcMeasure(Measure measure) {
        double result = 0;

        switch (measure) {
            case ACCURACY:
                result = calcAccuracy();
                break;
            case PRECISION:
                result = calcPrecision();
                break;
            case RECALL:
                result = calcRecall();
                break;
            case SPECIFICITY:
                result = calcSpecificity();
                break;
            case F1:
                result = calcF1();
                break;
        }

        System.out.println("Performance: " + result + " and Coverage: " + this.coverage);
        return result;
    }

    private double calcAccuracy() {
        return ((double) truePositives + (double) trueNegatives) / ((double) truePositives + (double) trueNegatives + (double) falsePositives + (double) falseNegatives);
    }

    private double calcPrecision() {
        return truePositives + falsePositives == 0 ? 0 : (double) truePositives / ((double) truePositives + (double) falsePositives);
    }

    private double calcRecall() {
        return truePositives + falseNegatives == 0 ? 0 : (double) truePositives / ((double) truePositives + (double) falseNegatives);
    }

    private double calcSpecificity() {
        return falsePositives + trueNegatives == 0 ? 0 :(double) trueNegatives / ((double) falsePositives + (double) trueNegatives);
    }

    private double calcF1() {

        double precision = calcPrecision();
        double recall = calcRecall();

        return precision + recall == 0 ? 0 : 2 * ((precision * recall) / (precision + recall));
    }

    /**
     * Calculating the values in the confusion matrix
     *
     * @param predictedValues the values predicted by the given ruleset
     * @param predictFunction the classification function
     * @param data all tabular instances
     */
    private void calcPerformance(List<Integer> predictedValues, Function<TabularInstance, Integer> predictFunction, TabularInstance[] data) {
        int predInstances = 0;
        int truePositives = 0;
        int falsePositives = 0;
        int trueNegatives = 0;
        int falseNegatives = 0;
        for (int i = 0; i < data.length; i++) {
            if (!predictedValues.get(i).equals(-1)) {
                predInstances++;
                if (predictFunction.apply(data[i]).equals(1) && predictedValues.get(i).equals(1))
                    truePositives++;
                else if (predictFunction.apply(data[i]).equals(0) && predictedValues.get(i).equals(0))
                    trueNegatives++;
                else if (predictFunction.apply(data[i]).equals(1) && predictedValues.get(i).equals(0))
                    falseNegatives++;
                else if (predictFunction.apply(data[i]).equals(0) && predictedValues.get(i).equals(1))
                    falsePositives++;
            }
        }

        this.coverage = (double) predInstances / (double) predictedValues.size();
        this.truePositives = truePositives;
        this.falsePositives = falsePositives;
        this.trueNegatives = trueNegatives;
        this.falseNegatives = falseNegatives;
    }

    public double getCoverage() {
        return coverage;
    }

    public void setCoverage(double coverage) {
        this.coverage = coverage;
    }

    public void setTruePositives(int truePositives) {
        this.truePositives = truePositives;
    }

    public void setFalsePositives(int falsePositives) {
        this.falsePositives = falsePositives;
    }

    public void setTrueNegatives(int trueNegatives) {
        this.trueNegatives = trueNegatives;
    }

    public void setFalseNegatives(int falseNegatives) {
        this.falseNegatives = falseNegatives;
    }

    public int getTruePositives() {
        return truePositives;
    }

    public int getFalsePositives() {
        return falsePositives;
    }

    public int getTrueNegatives() {
        return trueNegatives;
    }

    public int getFalseNegatives() {
        return falseNegatives;
    }
}
