package com.postlog.labs;

public class Vector3 extends Point {
    public static Vector3 zero = new Vector3(0, 0, 0);

    public Vector3(double x, double y, double z) {
        super(x, y, z);
    }
    public Vector3(Point point) { super(point.x, point.y, point.z); }

    public Vector3 add(Vector3 anotherVector) {
        return new Vector3(this.x + anotherVector.x, this.y + anotherVector.y, this.z + anotherVector.z);
    }

    public Vector3 subtract(Vector3 anotherVector) {
        return this.add(anotherVector.multiplyByScalar(-1));
    }

    public Vector3 cross(Vector3 anotherVector) {
        return new Vector3(
                this.y * anotherVector.z - this.z * anotherVector.y,
                this.z * anotherVector.x - this.x * anotherVector.z,
                this.x * anotherVector.y - this.y * anotherVector.x
        );
    }

    public double length() {
        return Math.sqrt(Math.pow(this.x, 2) + Math.pow(this.y, 2) + Math.pow(this.z, 2));
    }

    public Vector3 multiplyByScalar(double scalar) {
        return new Vector3(this.x * scalar, this.y * scalar, this.z * scalar);
    }

    @Override
    public String toString() {
        return "Vector3{x=" + this.x + ", y=" + this.y + ", z=" + this.z + "}";
    }

    @Override
    public boolean equals(Object object) {
        if (!(object instanceof Vector3)) return false;
        Vector3 anotherVector = (Vector3) object;
        return this.x == anotherVector.x && this.y == anotherVector.y && this.z == anotherVector.z;
    }
}
