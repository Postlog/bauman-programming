package com.postlog.labs;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class BigIterableNumber implements Iterable<Integer> {

    private final int[] numbers;

    public BigIterableNumber(int... numbers) {
        this.numbers = numbers;
    }

    @Override
    public Iterator<Integer> iterator() {
        return new BitsIterator();
    }

    private class BitsIterator implements Iterator<Integer> {
        private int position = 0;
        private final int[] singleBitsIndices;

        public BitsIterator() {
            singleBitsIndices = getSingleBitsIndices(numbers);
        }

        @Override
        public boolean hasNext() {
            return this.position < this.singleBitsIndices.length;
        }

        @Override
        public Integer next() {
            return this.singleBitsIndices[this.position++];
        }

        private int[] getSingleBitsIndices(int[] numbers) {
            List<Integer> indices = new ArrayList<>();
            int bitIndex = 0;
            for (int number : numbers) {
                for (int i = 0; i < 32; i++) {
                    if (getBitAtIndex(number, i)) {
                        indices.add(bitIndex);
                    }
                    bitIndex++;
                }
            }

            return indices.stream().mapToInt(x -> x).toArray();
        }

        private boolean getBitAtIndex(int number, int index) {
            if (index < 0 || index > 31) {
                throw new IllegalArgumentException("index argument must be in range [0; 31]");
            }
            return ((number >> (31 - index)) & 1) != 0;
        }
    }
}
