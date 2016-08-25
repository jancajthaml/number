package com.github.jancajthaml.number;

class IntegerNumber extends PreciseNumber {

    public IntegerNumber() {
        super(true, precision_digits);
    }

    public IntegerNumber(int precision) {
        super(true, precision);
    }

    public IntegerNumber(IntegerNumber in ) {
        super((PreciseNumber) in );
    }

    public IntegerNumber(double d, int precision) {
        super(true, precision);

        PreciseNumber mpt1 = new PreciseNumber(6, false);
        PreciseNumber mpt2 = new PreciseNumber(8, false);

        dmc(new Chunk(d), mpt1);
        infr(mpt1, this, mpt2, Math.min(maxnw - 2, nw));
    }

    public IntegerNumber(String str) {
        this(str, precision_digits);
    }

    public IntegerNumber(String str, int precision) {
        super(true, precision);

        int lnw = Math.min(nw, maxnw - 2);
        int nw2 = lnw + 2;

        PreciseNumber mpt1 = new PreciseNumber(nw2, false);
        PreciseNumber mpt2 = new PreciseNumber(nw2, false);

        dexc(str.toCharArray(), str.length(), mpt1, lnw);
        infr(mpt1, this, mpt2, lnw);
    }

    public IntegerNumber(RealNumber mpr) {
        this(mpr, precision_digits);
    }

    public IntegerNumber(RealNumber mpr, int precision) {
        super(true, precision);

        int lnw = Math.min(nw, maxnw - 2);
        PreciseNumber mpt1 = new PreciseNumber(lnw + 1, false);
        PreciseNumber mpt2 = new PreciseNumber(lnw + 2, false);

        eq(mpr, mpt1, lnw);
        infr(mpt1, this, mpt2, lnw);
    }

    public IntegerNumber assign(PreciseNumber ja) {
        if (ja != this) {
            if (ja.maxnw == this.maxnw && ja instanceof IntegerNumber) {
                PreciseNumber.eq(ja, this, Math.min(nw, this.maxnw - 1));
            } else {
                int nw1 = Math.min(nw, ja.maxnw - 1);
                int nw2 = Math.min(nw, maxnw - 2);
                RealNumber mpt1 = new RealNumber();
                RealNumber mpt2 = new RealNumber();

                eq(ja, mpt1, nw1);
                infr(mpt1, this, mpt2, nw2);
            }
        }
        return this;
    }

    public IntegerNumber add(IntegerNumber ja) {
        IntegerNumber res = new IntegerNumber();
        IntegerNumber mpt1 = new IntegerNumber();
        IntegerNumber mpt2 = new IntegerNumber();

        add(ja, this, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumber subtract(IntegerNumber ja) {
        IntegerNumber res = new IntegerNumber();
        IntegerNumber mpt1 = new IntegerNumber();
        IntegerNumber mpt2 = new IntegerNumber();

        sub(this, ja, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumber negate() {
        IntegerNumber res = new IntegerNumber();

        eq(this, res, nw);

        res.sign = !this.sign;

        return res;
    }

    public IntegerNumber multiply(IntegerNumber ja) {
        IntegerNumber res = new IntegerNumber();
        IntegerNumber mpt1 = new IntegerNumber();
        IntegerNumber mpt2 = new IntegerNumber();

        _mul(ja, this, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumber divide(IntegerNumber ja) {
        IntegerNumber res = new IntegerNumber();
        IntegerNumber mpt1 = new IntegerNumber();
        IntegerNumber mpt2 = new IntegerNumber();

        _div(this, ja, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumber mod(IntegerNumber ja) {
        IntegerNumber res = new IntegerNumber();
        IntegerNumber mpt1 = new IntegerNumber();
        IntegerNumber mpt2 = new IntegerNumber();
        IntegerNumber mpt3 = new IntegerNumber();

        _div(this, ja, mpt1, nw);
        infr(mpt1, mpt2, mpt3, nw);
        _mul(ja, mpt2, mpt1, nw);
        sub(this, mpt1, res, nw);

        return res;
    }

    public IntegerNumber abs() {
        IntegerNumber res = new IntegerNumber();

        eq(this, res, nw);

        res.sign = true;

        return res;
    }

    public IntegerNumber max(IntegerNumber val) {
        return (compare(this, val, nw) >= 0) ? this : val;
    }

    public IntegerNumber min(IntegerNumber val) {
        return (compare(this, val, nw) < 0) ? this : val;
    }

    public IntegerNumber sign(PreciseNumber val) {
        IntegerNumber res = new IntegerNumber();

        eq(this, res, nw);

        res.sign = val.sign;

        return res;
    }

    public IntegerNumber pow(IntegerNumber exponent) {
        IntegerNumber res = new IntegerNumber();
        IntegerNumber mpt1 = new IntegerNumber();
        IntegerNumber mpt2 = new IntegerNumber();

        _log(this, RealNumber.PI, RealNumber.LOG2, mpt1, nw);
        _mul(mpt1, exponent, mpt2, nw);
        _exp(mpt2, RealNumber.PI, RealNumber.LOG2, mpt1, nw);
        nint(mpt1, res, nw);

        return res;
    }

    public IntegerNumber pow(int exponent) {
        IntegerNumber res = new IntegerNumber(), mpt1 = new IntegerNumber();

        _npw(this, exponent, mpt1, nw);
        nint(mpt1, res, nw);

        return res;
    }

}