import com.postlog.labs.MaterialPoint2D;
import com.postlog.labs.MaterialPointCollection;


public class Test {
    public static void main(String[] args) {
        MaterialPointCollection points = new MaterialPointCollection();
        points.add(new MaterialPoint2D(0, 0, 10));
        points.add(new MaterialPoint2D(0, 0, 20));
        points.add(new MaterialPoint2D(3, 3, 10));
        points.add(new MaterialPoint2D(10, 10, 30));

        System.out.println("Peaks");
        points.peakStream().forEach(System.out::println);

        System.out.println("Mass center");
        System.out.println(points.getMassCenter(5));
    }
}

