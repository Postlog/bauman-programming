package com.postlog.labs;


class DigitArray {
    private final int[] digits;

    public DigitArray(int... digits) {
        for (int digit : digits) {
            validateNumber(digit);
        }
        this.digits = digits;
    }

    public int get(int index) {
        if (index < 0 || index >= digits.length)
            throw new IndexOutOfBoundsException(index);
        return this.digits[index];
    }

    public int count() {
        return this.digits.length;
    }

    public DigitArray padStart(int length) {
        if (length <= this.count()) return new DigitArray(this.digits.clone());
        int[] digits = new int[length];
        System.arraycopy(this.digits, 0, digits, length - this.digits.length, this.digits.length);
        return new DigitArray(digits);
    }

    private static void validateNumber(int number) {
        if (number < 0 || number > 9) {
            throw new IllegalArgumentException("number must be in range [0; 9]");
        }
    }
}


public class BigUnsignedDecimal {
    private final DigitArray digitArray;

    public BigUnsignedDecimal(int... digits) {
        this.digitArray = new DigitArray(digits);
    }

    public BigUnsignedDecimal add(BigUnsignedDecimal anotherNumber) {
        int maxDigitsCount = Math.max(this.digitArray.count(), anotherNumber.digitArray.count());

        DigitArray firstDigitArray = this.digitArray.padStart(maxDigitsCount),
                   secondDigitArray = anotherNumber.digitArray.padStart(maxDigitsCount);

        int[] summedDigits = new int[maxDigitsCount];

        int carry = 0, sum;
        for (int i = maxDigitsCount - 1; i >= 0; i--) {
            sum = firstDigitArray.get(i) + secondDigitArray.get(i) + carry;
            carry = sum / 10;
            sum %= 10;
            summedDigits[i] = sum;
        }

        if (carry != 0) {
            int[] summedDigitsTemp = summedDigits;
            summedDigits = new int[maxDigitsCount + 1];
            System.arraycopy(summedDigitsTemp, 0, summedDigits, 1, summedDigitsTemp.length);
            summedDigits[0] = carry;
        }

        return new BigUnsignedDecimal(summedDigits);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder(this.digitArray.count());

        for(int i = 0; i < this.digitArray.count(); i++) {
            builder.append(this.digitArray.get(i));
        }

        return builder.toString();
    }
}
