package LossFunctions;

import de.viadee.xai.anchor.adapter.tabular.TabularInstance;

import java.util.List;
import java.util.function.Function;

public class PerfomanceMeasures {

    double coverage;
    int truePositives;
    int falsePositives;
    int trueNegatives;
    int falseNegatives;

    public enum Measure {
        ACCURACY,
        PRECISION,
        RECALL
    }

    public PerfomanceMeasures(List<Integer> predictedValues, Function<TabularInstance, Integer> predictFunction, TabularInstance[] data) {
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
        }

        System.out.println("Performance: " + result + " and Coverage: " + this.coverage);
        return result;
    }

    private double calcAccuracy() {
        double accuracy = ((double) this.truePositives + (double) this.trueNegatives) / ((double) truePositives + (double) trueNegatives + (double) falsePositives + (double) falseNegatives);
        return accuracy;
    }

    private double calcPrecision() {
        double precision = (double) this.truePositives / ((double) truePositives + (double) falsePositives);
        return precision;
    }

    private double calcRecall() {
        double recall = (double) this.truePositives / ((double) truePositives + (double) falseNegatives);
        return recall;
    }


    /**
     * @param predictedValues
     * @param predictFunction
     * @param data
     * @return
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
