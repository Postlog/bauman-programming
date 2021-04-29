package com.postlog.labs;

public class MaterialPoint2D extends Vector2D {
    protected final double mass;

    public MaterialPoint2D(double x, double y, double mass) {
        super(x, y);
        this.mass = mass;
    }

    public double getMass() {
        return mass;
    }

    @Override
    public String toString() {
        return "MaterialPoint2D{mass=" + mass + ", x=" + this.x + ", y=" + this.y + "}";
    }
}
