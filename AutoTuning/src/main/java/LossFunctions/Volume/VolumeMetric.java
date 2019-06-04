package LossFunctions.Volume;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class VolumeMetric {

    private double hvValue = 0.0;
    private double[] referencePoint = {0.0, 0.0};

    public VolumeMetric() {
    }

    /**
     * @param referencePoint
     */
    public VolumeMetric(double[] referencePoint) {
        this.referencePoint = referencePoint;
    }

    /**
     * Method for returning only the dominant data points from a list of all data points
     *
     * @param allPoints
     * @return dominantPoints - List of only dominant data points
     */
    public List<Point> getDominantPoints(List<Point> allPoints) {

        List<Point> dominantPoints = allPoints;
        List<Point> nonDominantPoints = new ArrayList<>();

        if (dominantPoints.size() == 0 || dominantPoints.size() == 1) {
            return dominantPoints;
        }

        // for all data points check if they get strictly dominated by others, if yes mark them for removal
        for (Point p : dominantPoints) {
            for (Point q : dominantPoints) {
                if (p.getCoverage() <= q.getCoverage() && p.getPrecision() <= q.getPrecision() && !p.equals(q)) {
                    nonDominantPoints.add(p);
                }
            }
        }

        // remove non-dominant data points
        dominantPoints.removeAll(nonDominantPoints);

        return dominantPoints;
    }


    /**
     * Method for returning the volume of given dominant rules
     *
     * @param dominantPoints
     * @return volume
     */
    public double calcVolume(List<Point> dominantPoints) {

        double volume = 0.0;
        int pointCount = dominantPoints.size();

        if (pointCount == 0) {

        } else {

            dominantPoints.sort(Comparator.comparingDouble(Point::getPrecision).reversed());

            for (int i = 0; i < pointCount; i++) {

                Point currentPoint = dominantPoints.get(i);

                if (i == 0) {
                    volume += (referencePoint[0] - currentPoint.getCoverage()) * (referencePoint[1] - currentPoint.getPrecision());
                } else {
                    Point prevPoint = dominantPoints.get(i - 1);
                    volume += (referencePoint[0] - currentPoint.getCoverage()) * (referencePoint[1] - currentPoint.getPrecision()) - (referencePoint[0] - prevPoint.getCoverage()) * (referencePoint[1] - currentPoint.getPrecision());
                }
            }
        }
        this.hvValue = volume;
        return this.hvValue;
    }



//    public double calculateVolume(List<Point> population) {
//
//        double[] referencePointDouble = {0.0, 0.0};
//
//        Point referencePoint = new ArrayPoint(referencePointDouble);
//        WFGHypervolume hypervolume = new WFGHypervolume();
//
//        this.hvValue = hypervolume.computeHypervolume(population, referencePoint);
//
//        return this.hvValue;
//    }


    /**
     * Getter and Setter
     */
    public double[] getReferencePoint() {
        return referencePoint;
    }

    public void setReferencePoint(double[] referencePoint) {
        this.referencePoint = referencePoint;
    }

}
