package com.github.jancajthaml.number;

class IntegerNumberStub extends PreciseNumber {

    public IntegerNumberStub() {
        super(true, precision_digits);
    }

    public IntegerNumberStub(int precision) {
        super(true, precision);
    }

    public IntegerNumberStub(IntegerNumberStub in ) {
        super((PreciseNumber) in );
    }

    public IntegerNumberStub(double d, int precision) {
        super(true, precision);

        PreciseNumber mpt1 = new PreciseNumber(6, false);
        PreciseNumber mpt2 = new PreciseNumber(8, false);

        dmc(new Chunk(d), mpt1);
        infr(mpt1, this, mpt2, Math.min(maxnw - 2, nw));
    }

    public IntegerNumberStub(String str) {
        this(str, precision_digits);
    }

    public IntegerNumberStub(String str, int precision) {
        super(true, precision);

        int lnw = Math.min(nw, maxnw - 2);
        int nw2 = lnw + 2;

        PreciseNumber mpt1 = new PreciseNumber(nw2, false);
        PreciseNumber mpt2 = new PreciseNumber(nw2, false);

        dexc(str.toCharArray(), str.length(), mpt1, lnw);
        infr(mpt1, this, mpt2, lnw);
    }

    public IntegerNumberStub(RealNumber mpr) {
        this(mpr, precision_digits);
    }

    public IntegerNumberStub(RealNumber mpr, int precision) {
        super(true, precision);

        int lnw = Math.min(nw, maxnw - 2);
        PreciseNumber mpt1 = new PreciseNumber(lnw + 1, false);
        PreciseNumber mpt2 = new PreciseNumber(lnw + 2, false);

        eq(mpr, mpt1, lnw);
        infr(mpt1, this, mpt2, lnw);
    }

    public IntegerNumberStub assign(PreciseNumber ja) {
        if (ja != this) {
            if (ja.maxnw == this.maxnw && ja instanceof IntegerNumberStub) {
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

    public IntegerNumberStub add(IntegerNumberStub ja) {
        IntegerNumberStub res = new IntegerNumberStub();
        IntegerNumberStub mpt1 = new IntegerNumberStub();
        IntegerNumberStub mpt2 = new IntegerNumberStub();

        add(ja, this, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumberStub subtract(IntegerNumberStub ja) {
        IntegerNumberStub res = new IntegerNumberStub();
        IntegerNumberStub mpt1 = new IntegerNumberStub();
        IntegerNumberStub mpt2 = new IntegerNumberStub();

        sub(this, ja, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumberStub negate() {
        IntegerNumberStub res = new IntegerNumberStub();

        eq(this, res, nw);

        res.sign = !this.sign;

        return res;
    }

    public IntegerNumberStub multiply(IntegerNumberStub ja) {
        IntegerNumberStub res = new IntegerNumberStub();
        IntegerNumberStub mpt1 = new IntegerNumberStub();
        IntegerNumberStub mpt2 = new IntegerNumberStub();

        _mul(ja, this, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumberStub divide(IntegerNumberStub ja) {
        IntegerNumberStub res = new IntegerNumberStub();
        IntegerNumberStub mpt1 = new IntegerNumberStub();
        IntegerNumberStub mpt2 = new IntegerNumberStub();

        _div(this, ja, mpt1, nw);
        infr(mpt1, res, mpt2, nw);

        return res;
    }

    public IntegerNumberStub mod(IntegerNumberStub ja) {
        IntegerNumberStub res = new IntegerNumberStub();
        IntegerNumberStub mpt1 = new IntegerNumberStub();
        IntegerNumberStub mpt2 = new IntegerNumberStub();
        IntegerNumberStub mpt3 = new IntegerNumberStub();

        _div(this, ja, mpt1, nw);
        infr(mpt1, mpt2, mpt3, nw);
        _mul(ja, mpt2, mpt1, nw);
        sub(this, mpt1, res, nw);

        return res;
    }

    public IntegerNumberStub abs() {
        IntegerNumberStub res = new IntegerNumberStub();

        eq(this, res, nw);

        res.sign = true;

        return res;
    }

    public IntegerNumberStub max(IntegerNumberStub val) {
        return (compare(this, val, nw) >= 0) ? this : val;
    }

    public IntegerNumberStub min(IntegerNumberStub val) {
        return (compare(this, val, nw) < 0) ? this : val;
    }

    public IntegerNumberStub sign(PreciseNumber val) {
        IntegerNumberStub res = new IntegerNumberStub();

        eq(this, res, nw);

        res.sign = val.sign;

        return res;
    }

    public IntegerNumberStub pow(IntegerNumberStub exponent) {
        IntegerNumberStub res = new IntegerNumberStub();
        IntegerNumberStub mpt1 = new IntegerNumberStub();
        IntegerNumberStub mpt2 = new IntegerNumberStub();

        _log(this, RealNumber.PI, RealNumber.LOG2, mpt1, nw);
        _mul(mpt1, exponent, mpt2, nw);
        _exp(mpt2, RealNumber.PI, RealNumber.LOG2, mpt1, nw);
        nint(mpt1, res, nw);

        return res;
    }

    public IntegerNumberStub pow(int exponent) {
        IntegerNumberStub res = new IntegerNumberStub(), mpt1 = new IntegerNumberStub();

        _npw(this, exponent, mpt1, nw);
        nint(mpt1, res, nw);

        return res;
    }

}