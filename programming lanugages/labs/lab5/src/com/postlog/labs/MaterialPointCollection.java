package com.postlog.labs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class MaterialPointCollection {
    private final List<MaterialPoint2D> points = new ArrayList<>();

    public MaterialPointCollection(MaterialPoint2D... initialPoints) {
        this.points.addAll(Arrays.asList(initialPoints));
    }

    public void add(MaterialPoint2D point) {
        this.points.add(point);
    }

    public Optional<Vector2D> getMassCenter(double radius) {
        Vector2D center = new Vector2D(0, 0);
        if (this.points.stream().noneMatch(point -> point.getDistance(center) <= radius)) {
            return Optional.empty();
        }

        Vector2D summaryPoint = new Vector2D(0, 0);
        double fullMass = this.points.stream()
                .filter(point -> point.getDistance(center) <= radius)
                .map(point -> {
                    summaryPoint.add(point.multiplyByScalar(point.getMass()));
                    return point.getMass();
                }).reduce(0.0, Double::sum);
        return Optional.of(summaryPoint.multiplyByScalar(1.0 / fullMass));
    }

    public Stream<MaterialPoint2D> peakStream() {
        return this.getPeakPoints().stream();
    }

    public Stream<MaterialPoint2D> stream() {
        return this.points.stream();
    }

    public MaterialPointCollection getPeakPoints() {
        int pointsCount = this.points.size();

        if (pointsCount <= 1) {
            return new MaterialPointCollection(this.points.toArray(MaterialPoint2D[]::new));
        }

        List<MaterialPoint2D> peakPoints = new ArrayList<>();

        MaterialPoint2D firstPoint = this.points.get(0),
                secondPoint = this.points.get(1);

        if (firstPoint.getMass() > secondPoint.getMass()) {
            peakPoints.add(firstPoint);
        }


        if (pointsCount > 2) {
            MaterialPoint2D lastPoint = this.points.get(pointsCount - 1),
                    preLastPoint = this.points.get(pointsCount - 2);
            if (lastPoint.getMass() > preLastPoint.getMass()) {
                peakPoints.add(lastPoint);
            }
        }


        for (int i = 1; i < pointsCount - 1; i++) {
            MaterialPoint2D currentPoint = this.points.get(i),
                    previousPoint = this.points.get(i - 1),
                    nextPoint = this.points.get(i + 1);
            if (currentPoint.getMass() > previousPoint.getMass() &&
                    currentPoint.getMass() > nextPoint.getMass()) {
                peakPoints.add(currentPoint);
            }
        }

        return new MaterialPointCollection(peakPoints.toArray(MaterialPoint2D[]::new));
    }
}
