import com.postlog.labs.MaterialPoint;
import com.postlog.labs.Universe;
import com.postlog.labs.Vector3;

public class Test {
    public static void main(String[] args) throws Exception{
        Universe universe = new Universe();
        universe.addPoint(new MaterialPoint(10, 0, 0, new Vector3(-1, 1, 0)));

        Universe anotherUniverse = new Universe();
        anotherUniverse.addPoint(new MaterialPoint(0, 0, 0, new Vector3(-1, 1, 0)));

        System.out.println(universe.getDistance(anotherUniverse));
        System.out.println(universe.isConverge(anotherUniverse));
    }
}