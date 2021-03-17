package com.postlog.labs;

public class Point {
    protected final double x;
    protected final double y;
    protected final double z;


    public Point(double x, double y, double z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public double getDistance(Point anotherPoint) {
        return Math.sqrt(
                Math.pow(this.x - anotherPoint.getX(), 2) +
                Math.pow(this.y - anotherPoint.getY(), 2) +
                Math.pow(this.z - anotherPoint.getZ(), 2)
        );
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public double getZ() {
        return z;
    }

}
