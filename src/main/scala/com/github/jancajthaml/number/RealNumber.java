package com.github.jancajthaml.number;

class RealNumber extends PreciseNumber {

    public static RealNumber LOG2;
    public static RealNumber LOG10;
    public static RealNumber PI;
    public static RealNumber EPSILON;

    public RealNumber() {
        super(true, precision_digits);
    }

    public RealNumber(boolean b, int precision) {
        super(b, precision);
    }

    public RealNumber(int size, boolean b) {
        super(size, b);
    }

    public RealNumber(RealNumber in ) {
        super((PreciseNumber) in );
    }

    public RealNumber(double d) {
        super(d, precision_digits);
    }

    public RealNumber(double d, int precision) {
        super(d, precision);
    }

    public RealNumber(String str) {
        super(str, precision_digits);
    }

    public RealNumber(String str, int precision) {
        super(str, precision);
    }

    public RealNumber(IntegerNumber in ) {
        super((PreciseNumber) in );
    };

    RealNumber(int size) {
        super(size, false);
    }

    public RealNumber assign(PreciseNumber ja) {
        if (ja != this)
            eq(ja, this, Math.min(nw, this.maxnw - 1));

        return this;
    }

    public RealNumber add(RealNumber ja) {
        RealNumber res = new RealNumber();

        add(this, ja, res, nw);

        return res;
    }

    public RealNumber subtract(RealNumber ja) {
        RealNumber res = new RealNumber();

        sub(this, ja, res, nw);

        return res;
    }

    public RealNumber negate() {
        RealNumber res = new RealNumber();

        eq(this, res, nw);

        res.sign = !this.sign;

        return res;
    }

    public RealNumber multiply(RealNumber ja) {
        RealNumber res = new RealNumber();

        _mul(this, ja, res, nw);

        return res;
    }

    public RealNumber divide(RealNumber ja) {
        RealNumber res = new RealNumber();

        _div(this, ja, res, nw);

        return res;
    }

    public RealNumber abs() {
        RealNumber res = new RealNumber();

        eq(this, res, nw);

        res.sign = true;

        return res;
    }

    public RealNumber max(RealNumber val) {
        return (compare(this, val, nw) >= 0) ? this : val;
    }

    public RealNumber min(RealNumber val) {
        return (compare(this, val, nw) < 0) ? this : val;
    }

    public RealNumber sign(PreciseNumber val) {
        RealNumber res = new RealNumber();

        eq(this, res, nw);

        res.sign = val.sign;

        return res;
    }

    public RealNumber pow(PreciseNumber exponent) {
        RealNumber res = new RealNumber();
        PreciseNumber mpt1 = new PreciseNumber();
        PreciseNumber mpt2 = new PreciseNumber();

        _log(this, PI, LOG2, mpt1, nw);
        _mul(mpt1, exponent, mpt2, nw);
        _exp(mpt2, PI, LOG2, mpt1, nw);

        return res;
    }

    public RealNumber pow(int exponent) {
        RealNumber res = new RealNumber();

        _npw(this, exponent, res, nw);

        return res;
    }

    public RealNumber pow(double exponent) {
        RealNumber res = new RealNumber();
        PreciseNumber mpt1 = new PreciseNumber();
        PreciseNumber mpt2 = new PreciseNumber();

        _log(this, PI, LOG2, mpt1, nw);
        muld(mpt1, new Chunk(exponent), mpt2, nw);
        _exp(mpt2, PI, LOG2, res, nw);

        return res;
    }

    public RealNumber sqrt() {
        RealNumber res = new RealNumber();

        _sqr(this, res, nw);

        return res;
    }

    static {
        PI = new RealNumber(mp21, false);
        _pi(PI, nw + 1);

        LOG2 = new RealNumber(mp21, false);

        PreciseNumber t2 = new PreciseNumber(6, false);

        dmc(new Chunk(2), t2);
        _log(t2, PI, LOG2, LOG2, nw + 1);

        LOG10 = new RealNumber(mp21, false);

        dmc(new Chunk(10), t2);
        _log(t2, PI, LOG2, LOG10, nw + 1);

        EPSILON = new RealNumber(mp21, false);

        dmc(new Chunk(10), t2);
        _npw(t2, ellog10, EPSILON, nw + 1);

        PI.number_words--;
        LOG2.number_words--;
        LOG10.number_words--;
        EPSILON.number_words--;
    }

}