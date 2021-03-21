package com.postlog.labs;

import java.util.ArrayList;
import java.util.List;

public class MoneyExchange implements Comparable<MoneyExchange> {
    private static final int[] nominalValues = {1, 2, 5, 10, 50, 100, 200, 500, 1000, 2000, 5000};

    private final int[] exchange;
    private final int sum;

    public MoneyExchange(int sum) {
        this.sum = sum;
        this.exchange = MoneyExchange.makeExchange(sum);
    }

    public int getSum() {
        return sum;
    }

    public static int[] makeExchange(int sum) {
        if (sum <= 0)
            throw new IllegalArgumentException("sum argument must be greater then 0");

        List<Integer> exchange = new ArrayList<>();

        while(sum > 0) {
            int nominal = getNearestNominal(sum);
            exchange.add(nominal);
            sum -= nominal;
        }

        return exchange.stream().mapToInt(i->i).toArray();
    }


    @Override
    public int compareTo(MoneyExchange o) {
        if (this.exchange.length > o.exchange.length) {
            return 1;
        } else if (this.exchange.length < o.exchange.length) {
            return -1;
        }
        return 0;
    }

    @Override
    public String toString() {
        StringBuilder exchangeRepresentation = new StringBuilder(this.exchange.length);
        exchangeRepresentation.append("MoneyExchange{sum=");
        exchangeRepresentation.append(this.sum);
        exchangeRepresentation.append(", exchange={");
        for(int value : this.exchange) {
            exchangeRepresentation.append(value);
            exchangeRepresentation.append(", ");
        }
        exchangeRepresentation.delete(exchangeRepresentation.length() - 2, exchangeRepresentation.length());
        exchangeRepresentation.append("}}\n");
        return exchangeRepresentation.toString();
    }

    private static int getNearestNominal(int sum) {

        for(int i = MoneyExchange.nominalValues.length - 1; i >= 0; i--) {
            int nominal = MoneyExchange.nominalValues[i];
            if (sum >= nominal) return nominal;
        }

        throw new IllegalArgumentException("sum argument must be greater then 0");
    }
}
