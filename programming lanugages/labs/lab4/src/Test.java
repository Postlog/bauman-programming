import com.postlog.labs.BigIterableNumber;

public class Test {
    public static void main(String[] args) {                               //  0..31     32..63,   64...95
        BigIterableNumber number = new BigIterableNumber(1, 2, 4); // 000..001, 000..010, 000..100
        for(int index : number) {
            System.out.println(index); // 31 62 93
        }
    }
}

