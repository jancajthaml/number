package com.github.jancajthaml.number;

class RealNumberStub extends PreciseNumber {

    public static RealNumberStub LOG2;
    public static RealNumberStub LOG10;
    public static RealNumberStub PI;
    public static RealNumberStub EPSILON;

    public RealNumberStub() {
        super(true, precision_digits);
    }

    public RealNumberStub(boolean b, int precision) {
        super(b, precision);
    }

    public RealNumberStub(int size, boolean b) {
        super(size, b);
    }

    public RealNumberStub(RealNumberStub in ) {
        super((PreciseNumber) in );
    }

    public RealNumberStub(double d) {
        super(d, precision_digits);
    }

    public RealNumberStub(double d, int precision) {
        super(d, precision);
    }

    public RealNumberStub(String str) {
        super(str, precision_digits);
    }

    public RealNumberStub(String str, int precision) {
        super(str, precision);
    }

    public RealNumberStub(IntegerNumber in ) {
        super((PreciseNumber) in );
    };

    RealNumberStub(int size) {
        super(size, false);
    }

    public RealNumberStub assign(PreciseNumber ja) {
        if (ja != this)
            eq(ja, this, Math.min(nw, this.maxnw - 1));

        return this;
    }

    public RealNumberStub add(RealNumberStub ja) {
        RealNumberStub res = new RealNumberStub();

        add(this, ja, res, nw);

        return res;
    }

    public RealNumberStub subtract(RealNumberStub ja) {
        RealNumberStub res = new RealNumberStub();

        sub(this, ja, res, nw);

        return res;
    }

    public RealNumberStub negate() {
        RealNumberStub res = new RealNumberStub();

        eq(this, res, nw);

        res.sign = !this.sign;

        return res;
    }

    public RealNumberStub multiply(RealNumberStub ja) {
        RealNumberStub res = new RealNumberStub();

        _mul(this, ja, res, nw);

        return res;
    }

    public RealNumberStub divide(RealNumberStub ja) {
        RealNumberStub res = new RealNumberStub();

        _div(this, ja, res, nw);

        return res;
    }

    public RealNumberStub abs() {
        RealNumberStub res = new RealNumberStub();

        eq(this, res, nw);

        res.sign = true;

        return res;
    }

    public RealNumberStub max(RealNumberStub val) {
        return (compare(this, val, nw) >= 0) ? this : val;
    }

    public RealNumberStub min(RealNumberStub val) {
        return (compare(this, val, nw) < 0) ? this : val;
    }

    public RealNumberStub sign(PreciseNumber val) {
        RealNumberStub res = new RealNumberStub();

        eq(this, res, nw);

        res.sign = val.sign;

        return res;
    }

    public RealNumberStub pow(PreciseNumber exponent) {
        RealNumberStub res = new RealNumberStub();
        PreciseNumber mpt1 = new PreciseNumber();
        PreciseNumber mpt2 = new PreciseNumber();

        _log(this, PI, LOG2, mpt1, nw);
        _mul(mpt1, exponent, mpt2, nw);
        _exp(mpt2, PI, LOG2, mpt1, nw);

        return res;
    }

    public RealNumberStub pow(int exponent) {
        RealNumberStub res = new RealNumberStub();

        _npw(this, exponent, res, nw);

        return res;
    }

    public RealNumberStub pow(double exponent) {
        RealNumberStub res = new RealNumberStub();
        PreciseNumber mpt1 = new PreciseNumber();
        PreciseNumber mpt2 = new PreciseNumber();

        _log(this, PI, LOG2, mpt1, nw);
        muld(mpt1, new Chunk(exponent), mpt2, nw);
        _exp(mpt2, PI, LOG2, res, nw);

        return res;
    }

    public RealNumberStub sqrt() {
        RealNumberStub res = new RealNumberStub();

        _sqr(this, res, nw);

        return res;
    }

    static {
        PI = new RealNumberStub(mp21, false);
        _pi(PI, nw + 1);

        LOG2 = new RealNumberStub(mp21, false);

        PreciseNumber t2 = new PreciseNumber(6, false);

        dmc(new Chunk(2), t2);
        _log(t2, PI, LOG2, LOG2, nw + 1);

        LOG10 = new RealNumberStub(mp21, false);

        dmc(new Chunk(10), t2);
        _log(t2, PI, LOG2, LOG10, nw + 1);

        EPSILON = new RealNumberStub(mp21, false);

        dmc(new Chunk(10), t2);
        _npw(t2, ellog10, EPSILON, nw + 1);

        PI.number_words--;
        LOG2.number_words--;
        LOG10.number_words--;
        EPSILON.number_words--;
    }

}