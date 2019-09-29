package evaluationMetrics;

import org.junit.Assert;
import org.junit.Test;

public class PerformanceMeasuresTest {

    @Test
    public void testCalcMeasureAcc() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();
        performance.setTruePositives(5);
        performance.setTrueNegatives(5);
        performance.setFalsePositives(5);
        performance.setFalseNegatives(5);

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.ACCURACY);

        // Then
        Assert.assertEquals(10D/20D, result, 0);
    }


    @Test
    public void testCalcMeasureRecall() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();
        performance.setTruePositives(5);
        performance.setFalseNegatives(5);

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.RECALL);

        // Then
        Assert.assertEquals(5D/10D, result, 0);
    }

    @Test
    public void testCalcMeasureRecallNull() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.RECALL);

        // Then
        Assert.assertEquals(0D, result, 0);
    }


    @Test
    public void testCalcMeasurePrecision() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();
        performance.setTruePositives(5);
        performance.setFalsePositives(5);

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.PRECISION);

        // Then
        Assert.assertEquals(5D/10D, result, 0);
    }

    @Test
    public void testCalcMeasurePrecisionNull() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.PRECISION);

        // Then
        Assert.assertEquals(0D, result, 0);
    }

    @Test
    public void testCalcMeasureSpec() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();
        performance.setTrueNegatives(5);
        performance.setFalsePositives(5);

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.SPECIFICITY);

        // Then
        Assert.assertEquals(5D/10D, result, 0);
    }

    @Test
    public void testCalcMeasureSpecNull() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.SPECIFICITY);

        // Then
        Assert.assertEquals(0D, result, 0);
    }

    @Test
    public void testCalcMeasureF1Null() {
        // Given
        PerformanceMeasures performance = new PerformanceMeasures();

        // When
        double result = performance.calcMeasure(PerformanceMeasures.Measure.F1);

        // Then
        Assert.assertEquals(0D, result, 0);
    }
}