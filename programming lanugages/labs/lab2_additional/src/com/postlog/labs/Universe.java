package com.postlog.labs;

public class Universe {

    private double pointsXSum = 0;
    private double pointsYSum = 0;
    private double pointsZSum = 0;
    private int pointsCount = 0;
    private Vector3 velocitySum = Vector3.zero;

    public void addPoint(MaterialPoint point) {
        this.pointsCount++;
        this.pointsXSum += point.getX();
        this.pointsYSum += point.getY();
        this.pointsZSum += point.getZ();
        this.velocitySum = velocitySum.add(point.velocity);
    }

    public int getSize() {
        return this.pointsCount;
    }

    public double getDistance(Universe anotherUniverse) throws Exception {
        if (this.pointsCount == 0 || anotherUniverse.pointsCount == 0)
            throw new Exception("universe must have at least 1 point");

        Point center = this.getCenter(),
                anotherCenter = anotherUniverse.getCenter();

        return center.getDistance(anotherCenter);
    }

    public Vector3 getVelocity() {
        return this.velocitySum.multiplyByScalar(1.0 / this.pointsCount);
    }

    public boolean isConverge(Universe anotherUniverse) {
        Vector3 center = new Vector3(this.getCenter()),
                anotherCenter = new Vector3(anotherUniverse.getCenter());
        Vector3 velocity = this.getVelocity(),
                anotherVelocity = anotherUniverse.getVelocity();

        double t = center.subtract(anotherCenter).cross(velocity).length() /
                    anotherVelocity.cross(velocity).length(),

               u = center.subtract(anotherCenter).cross(anotherVelocity).length() /
                    anotherVelocity.cross(velocity).length();

        double crossProductLength = this.getVelocity().cross(anotherUniverse.getVelocity()).length();

        return crossProductLength != 0 && t >= 0 && u >= 0;

    }

    private Point getCenter() {
        return new Point(this.pointsXSum / this.pointsCount, this.pointsYSum / this.pointsCount,
                this.pointsZSum / this.pointsCount);
    }
}
