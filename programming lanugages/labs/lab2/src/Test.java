import com.postlog.labs.BigUnsignedDecimal;


public class Test {
    public static void main(String[] args) {
        BigUnsignedDecimal num1 = new BigUnsignedDecimal(1, 0, 0),
                           num2 = new BigUnsignedDecimal(1, 0, 0, 0);
        System.out.println(num2.add(num1)); // 9894
    }
}



