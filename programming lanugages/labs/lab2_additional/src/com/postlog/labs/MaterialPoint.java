package com.postlog.labs;

public class MaterialPoint extends Point {

    protected final Vector3 velocity;

    public MaterialPoint(double x, double y, double z, Vector3 velocity) {
        super(x, y, z);
        this.velocity = velocity;
    }
}
