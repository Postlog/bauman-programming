import com.postlog.labs.MoneyExchange;

import java.util.Arrays;

public class Test {
    public static void main(String[] args) {
        MoneyExchange[] exchanges = new MoneyExchange[10];
        for (int i = 0; i < 10; i++) {
            exchanges[i] = new MoneyExchange(getRandomIntInRange(3, 12345));
        }
        Arrays.sort(exchanges);

        for(MoneyExchange exchange : exchanges) {
            System.out.println(exchange);
        }

    }
    public static int getRandomIntInRange(int min, int max) {
        return (int) (Math.random() * ((max - min) + 1)) + min;
    }
}



