package LossFunctions;

import LossFunctions.Volume.Point;
import LossFunctions.Volume.VolumeMetric;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.*;

public class VolumeMetricTest {

    @Test
    public void calcVolumeForEmpty() {

        // Given
        List<Point> list = new ArrayList<>();
        VolumeMetric test = new VolumeMetric();

        // When
        double result = test.calcVolume(list);

        // Then
        assertEquals(0.0, result, 0);
    }

    @Test
    public void calcVolumeForSingle() {

        // Given
        Point a = new Point(0.8, 0.6);
        List<Point> list = new ArrayList<>();
        list.add(a);
        VolumeMetric test = new VolumeMetric();

        // When
        double result = test.calcVolume(list);

        // Then
        assertEquals(a.getCoverage() * a.getPrecision(), result, 0);
    }

    @Test
    public void calcVolumeForTwo() {

        // Given
        Point a = new Point(0.2, 0.8);
        Point b = new Point(0.8, 0.2);

        List<Point> list = new ArrayList<>();

        list.add(a);
        list.add(b);

        VolumeMetric test = new VolumeMetric();

        // When
        double result = test.calcVolume(list);
        double calc = a.getCoverage() * a.getPrecision() + b.getCoverage() * b.getPrecision() - a.getCoverage() * b.getPrecision();

        // Then
        assertEquals(calc, result, 0);
    }

    @Test
    public void calcVolumeForMultiple() {

        // Given
        Point a = new Point(0.2, 0.8);
        Point b = new Point(0.3, 0.7);
        Point c = new Point(0.7, 0.3);
        Point d = new Point(0.8, 0.2);

        List<Point> list = new ArrayList<>();

        list.add(a);
        list.add(b);
        list.add(c);
        list.add(d);

        VolumeMetric test = new VolumeMetric();

        // When
        double result = test.calcVolume(list);
        double calc = 0.37;

        // Then
        assertEquals(calc, result, 0.00001);
    }

    @Test
    public void calcVolumeForMultipleUnsorted() {

        // Given
        Point a = new Point(0.2, 0.8);
        Point b = new Point(0.3, 0.7);
        Point c = new Point(0.7, 0.3);
        Point d = new Point(0.8, 0.2);

        List<Point> list = new ArrayList<>();

        list.add(c);
        list.add(a);
        list.add(d);
        list.add(b);

        VolumeMetric test = new VolumeMetric();

        // When
        double result = test.calcVolume(list);
        double calc = 0.37;

        // Then
        assertEquals(calc, result, 0.00001);
    }

    @Test
    public void getDominantPointsEmpty() {

        // Given
        List<Point> list = new ArrayList<>();
        VolumeMetric test = new VolumeMetric();

        // When
        List<Point> result = test.getDominantPoints(list);

        // Then
        assertTrue(result.isEmpty());
    }

    @Test
    public void getDominantPointsSingle() {

        // Given
        Point a = new Point(0.2, 0.8);
        List<Point> list = new ArrayList<>();
        list.add(a);
        VolumeMetric test = new VolumeMetric();

        // When
        List<Point> result = test.getDominantPoints(list);

        // Then
        assertEquals(list, result);
    }

    @Test
    public void getDominantPointsTwo() {

        // Given
        Point a = new Point(0.2, 0.8);
        Point b = new Point(0.1, 0.7);

        List<Point> list = new ArrayList<>();

        list.add(a);
        list.add(b);

        VolumeMetric test = new VolumeMetric();

        // When
        List<Point> result = test.getDominantPoints(list);

        // Then
        assertEquals(Arrays.asList(a), result);
    }

    @Test
    public void getDominantPointsMultiple() {

        // Given
        Point a = new Point(0.2, 0.8);
        Point b = new Point(0.1, 0.7);
        Point c = new Point(0.7, 0.1);
        Point d = new Point(0.8, 0.2);

        List<Point> list = new ArrayList<>();

        list.add(a);
        list.add(b);
        list.add(c);
        list.add(d);

        VolumeMetric test = new VolumeMetric();

        // When
        List<Point> result = test.getDominantPoints(list);

        // Then
        assertEquals(Arrays.asList(a,d), result);
    }

    //    @Test
//    public void testEmptyCalculateVolume() {
//
//        // Given
//        List<Point> list = new ArrayList<>();
//        VolumeMetric test = new VolumeMetric();
//
//        // When
//        double result = test.calculateVolume(list);
//
//        // Then
//        assertEquals(0.0,result,0);
//
//    }
//
//    @Test
//    public void testSingleCalculateVolume() {
//
//        // Given
//        Point a = new Point(0.8,0.6);
//        List<Point> list = new ArrayList<>();
//        list.add(a);
//        VolumeMetric test = new VolumeMetric();
//
//        // When
//        double result = test.calculateVolume(list);
//
//        // Then
//        assertEquals( a.getCoverage()*a.getPrecision(),result,0);
//
//    }
//
//    @Test
//    public void testCalculateVolume() {
//
//        // Given
//        Point a = new Point(0.8,0.2);
//        Point b = new Point(0.2,0.8);
//
//        List<Point> list = new ArrayList<>();
//
//        list.add(a);
//        list.add(b);
//
//        VolumeMetric test = new VolumeMetric();
//
//
//        // When
//        double result = test.calculateVolume(list);
//        double calc = a.getPrecision()*a.getCoverage()+b.getPrecision()*b.getCoverage()-a.getCoverage()*b.getPrecision();
//
//        // Then
//        assertEquals(calc, result, 0);
//    }
}
