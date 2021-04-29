package com.postlog.labs;

public class Vector2D {
    protected double x;
    protected double y;

    public Vector2D(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }

    public double getDistance(Vector2D anotherPoint) {
        double xDistance = Math.abs(this.x - anotherPoint.x);
        double yDistance = Math.abs(this.y - anotherPoint.y);
        return Math.sqrt(Math.pow(xDistance, 2) + Math.pow(yDistance, 2));
    }

    public Vector2D add(Vector2D anotherVector) {
        this.x += anotherVector.x;
        this.y += anotherVector.y;
        return this;
    }

    public Vector2D multiplyByScalar(double scalar) {
        this.x *= scalar;
        this.y *= scalar;
        return this;
    }

    @Override
    public String toString() {
        return "Vector2D{x=" + x + ", y=" + y + '}';
    }
}
