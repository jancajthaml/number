package com.github.jancajthaml.number;

class PreciseNumber implements Cloneable {

    static final double AL2 = 0.301029995663981195;
    static final double CL2 = 1.4426950408889633;
    static final double CPI = 3.141592653589793;
    static final double ALT = 0.693147180559945309;
    static final double LOGE10 = 2.302585092994046;
    static final int NIT = 3;
    static final int N30 = 1073741824;

    private static int precision_current = 0;

    static int pointer = 0;
    static int round = 0;
    static int precision_digits_current = 0;
    static int precision_words = 0;
    static int precision_digits = 0;
    static int precision_digits_out = 0;
    static int ellog10 = 0;
    static int mp2 = 0;
    static int mp21 = 0;
    static int nw = 0;
    static Complex[] uu1 = null;
    static Complex[] uu2 = null;

    static {
        setMaximumPrecision(100);
    }

    public static final void setMaximumPrecision(int p) {
        if (p != precision_current) {
            precision_current = Math.min(Math.abs(p), 3600);
            pointer = 7;
            round = 1;
            precision_digits = precision_current;
            precision_digits_out = 10000000;
            ellog10 = 10 - precision_digits;
            precision_words = (int)((precision_digits / 7.224719896) + 1);
            mp2 = precision_words + 2;
            mp21 = mp2 + 1;
            precision_digits_current = precision_digits_out;
            nw = precision_words;
            int n = nw + 1;
            double ti = 0.0;
            int j = 0;
            int i = 0;
            double t1 = 0.75 * n;
            int m = (int)(CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14);
            int mq = m + 2;
            int nq = (int)(Math.pow(2, mq));
            uu1 = new Complex[nq];
            uu1[0] = new Complex(mq, 0);
            int ku = 1;
            int ln = 1;

            for (j = 1; j <= mq; j++) {
                t1 = Math.PI / ln;
                for (i = 0; i <= ln - 1; i++) {
                    ti = i * t1;
                    uu1[i + ku] = new Complex(Math.cos(ti), Math.sin(ti));
                }

                ku += ln;
                ln <<= 1;
            }

            double tpn = 0.0;
            int k = 0;
            t1 = 0.75 * n;
            uu2 = new Complex[mq + nq];
            ku = mq + 1;
            uu2[0] = new Complex(mq, 0);
            int mm = 0;
            int nn = 0;
            int mm1 = 0;
            int mm2 = 0;
            int nn1 = 0;
            int nn2 = 0;
            int iu = 0;

            for (k = 2; k <= mq - 1; k++) {
                uu2[k - 1] = new Complex(ku, 0);
                mm = k;
                nn = (int)(Math.pow(2, mm));
                mm1 = (mm + 1) / 2;
                mm2 = mm - mm1;
                nn1 = (int)(Math.pow(2, mm1));
                nn2 = (int)(Math.pow(2, mm2));
                tpn = 2.0 * Math.PI / nn;

                for (j = 0; j < nn2; j++) {
                    for (i = 0; i < nn1; i++) {
                        iu = ku + i + j * nn1;
                        t1 = tpn * i * j;
                        uu2[iu - 1] = new Complex(Math.cos(t1), Math.sin(t1));
                    }
                }

                ku += nn;

            }
        }
    }

    static final double nint(double x) {
        return (x < 0) ? Math.ceil(x - .5) : Math.floor(x + .5);
    }

    static final double fSign(double a, double b) {
        return (b >= 0 ? Math.abs(a) : -Math.abs(a));
    }

    static final double log10(double val) {
        return (Math.log(val) / LOGE10);
    }

    static final int precisionToSize(int precision) {
        int maxnw = (int)((precision / 7.224719896) + 4);
        return (maxnw < 8) ? 8 : maxnw;
    }


    int maxnw = 0;
    boolean sign = false;
    int number_words = 0;
    int exponent = 0;
    public float mantissa[] = null;
    static double t30 = 0;
    static double r30 = 0;
    static double sd = 314159265;
    static Integer randMutex = new Integer(0);

    PreciseNumber() {
        this(mp2, false);
    }

    PreciseNumber(PreciseNumber in ) {
        maxnw = in .maxnw;

        if (maxnw > 0) {
            mantissa = new float[maxnw];

            eq( in , this, nw);
        } else {
            exponent = in .exponent;
            sign = in .sign;
            number_words = in .number_words;
            mantissa = null;
        }
    }

    PreciseNumber(int maxNW, boolean b) {

        maxnw = maxNW;
        sign = true;
        number_words = 0;
        exponent = 0;

        if (maxnw >= 1) {
            mantissa = new float[maxnw];
            mantissa[0] = 0;
        } else mantissa = null;
    }

    PreciseNumber(boolean b, int precision) {
        maxnw = (precisionToSize(precision));
        mantissa = new float[maxnw];
        mantissa[0] = 0;
        sign = true;
        number_words = 0;
        exponent = 0;
    }

    PreciseNumber(double ia, int precision) {
        maxnw = precisionToSize(precision);
        mantissa = new float[maxnw];

        dmc(new Chunk(ia), this);
    }

    PreciseNumber(String str) {
        this(str, precision_digits);
    }

    PreciseNumber(String str, int precision) {
        char temp[] = str.toCharArray();
        maxnw = precisionToSize(precision);
        mantissa = new float[maxnw];

        dexc(temp, temp.length, this, Math.min(nw, maxnw - 2));
    }

    public Object clone() {
        return new PreciseNumber(this);
    }

    public String toString() {
        StringBuffer res = new StringBuffer();
        char az[] = new char[precision_digits + 100];
        int nd = mpouts(this, precision_digits_current, az, nw);
        int i = 0;

        for (i = 0; i < nd; i++) res.append(az[i]);
        String old = res.toString();
        int xx = old.indexOf("x");
        String exponent = old.substring(old.indexOf("^") + 1, xx).trim();
        int ex = Integer.parseInt(exponent);

        return old.substring(xx + 1).trim() + (ex != 0 ? ("e" + ((ex < 0) ? "" : "+") + exponent) : "");
    }

    public int getExponent() {
        StringBuffer res = new StringBuffer();
        char az[] = new char[precision_digits + 100];
        int nd = mpouts(this, precision_digits_current, az, nw);
        int i = 0;

        for (i = 0; i < nd; i++) res.append(az[i]);

        String old = res.toString();
        int hat = old.indexOf("^");
        int xx = old.indexOf("x");
        String exponent = old.substring(hat + 1, xx).trim();

        return Integer.parseInt(exponent);
    }

    public boolean equals(Object o) {
        if (o == this) {
          return true;
        }

        try {
            return 0 == compare(this, (PreciseNumber) o, nw);
        } catch (ClassCastException cce) {
            return false;
        }
    }

    public int compareTo(Object o) {
        return compare(this, (PreciseNumber) o, nw);
    }

    public boolean isNegative() {
        return (compareTo(new PreciseNumber(true, 10)) < 0);
    }

    public double doubleValue() {
        return this.toDPE().value();
    }

    public float floatValue() {
        return (float) this.toDPE().value();
    }

    public int intValue() {
        return (int) this.toDPE().value();
    }

    public long longValue() {
        return (long) this.toDPE().value();
    }

    public short shortValue() {
        return (short) this.toDPE().value();
    }

    public static int getPrecisionInDigits() {
        return precision_digits;
    }

    static void add(PreciseNumber a, PreciseNumber b, PreciseNumber c, int lnw) {
        int i = 0;
        int na = Math.min(a.number_words, lnw);
        int nb = Math.min(b.number_words, lnw);
        double d[] = new double[lnw + 5];
        double db = (a.sign == b.sign) ? 1.0 : -1.0;
        int ixa = a.exponent;
        int ixb = b.exponent;
        int ish = ixa - ixb;
        int nd = 0;
        int ixd = 0;

        if (ish >= 0) {
            int m1 = Math.min(na, ish);
            int m2 = Math.min(na, nb + ish);
            int m3 = na;
            int m4 = Math.min(Math.max(na, ish), lnw + 1);
            int m5 = Math.min(Math.max(na, nb + ish), lnw + 1);
            d[0] = 0;
            d[1] = 0;

            for (i = 0; i < m1; i++) d[i + 2] = a.mantissa[i];
            for (i = m1; i < m2; i++) d[i + 2] = a.mantissa[i] + db * b.mantissa[i - ish];
            for (i = m2; i < m3; i++) d[i + 2] = a.mantissa[i];
            for (i = m3; i < m4; i++) d[i + 2] = 0.0;
            for (i = m4; i < m5; i++) d[i + 2] = db * b.mantissa[i - ish];

            nd = m5;
            ixd = ixa;
            d[nd + 2] = 0.0;
            d[nd + 3] = 0.0;
        } else {
            int nsh = -ish;
            int m1 = Math.min(nb, nsh);
            int m2 = Math.min(nb, na + nsh);
            int m3 = nb;
            int m4 = Math.min(Math.max(nb, nsh), lnw + 1);
            int m5 = Math.min(Math.max(nb, na + nsh), lnw + 1);
            d[0] = 0;
            d[1] = 0;

            for (i = 0; i < m1; i++) d[i + 2] = db * b.mantissa[i];
            for (i = m1; i < m2; i++) d[i + 2] = a.mantissa[i - nsh] + db * b.mantissa[i];
            for (i = m2; i < m3; i++) d[i + 2] = db * b.mantissa[i];
            for (i = m3; i < m4; i++) d[i + 2] = 0.0;
            for (i = m4; i < m5; i++) d[i + 2] = a.mantissa[i - nsh];

            nd = m5;
            ixd = ixb;
            d[nd + 2] = 0.0;
            d[nd + 3] = 0.0;
        }

        d[0] = fSign(nd, a.sign ? 1 : -1);
        d[1] = ixd;

        mpnorm(d, c, lnw);
    }

    static void cbrt(PreciseNumber a, PreciseNumber b, int lnw) {
        int lnw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(lnw3, false);
        PreciseNumber sk1 = new PreciseNumber(lnw3, false);
        PreciseNumber sk2 = new PreciseNumber(lnw3, false);
        int na = Math.min(a.number_words, lnw);

        if (na == 0) {
            zero(b);
            return;
        }

        if (a.sign == false) throw new ArithmeticException("mpcbrt: argument is negative --> " + a);

        int nws = lnw;
        Chunk t1 = new Chunk();
        t1.a = lnw;
        int mq = (int)(CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14); // *cast*

        lnw++;

        mul(a, a, sk0, lnw);
        mdc(a, t1);

        Chunk t2 = new Chunk();
        t2.n = -(t1.n << 1) / 3;
        t2.a = Math.pow((t1.a * Math.pow(2.0, (t1.n + 3.0 * t2.n / 2.0))), (-2.0 / 3.0));

        dmc(t2, b);

        f.sign = true;
        f.number_words = 1;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;
        lnw = 3;
        int iq = 0;

        for (int k = 2; k <= mq - 1; k++) {
            int nw1 = lnw;
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            int nw2 = lnw;
            boolean cont = true;

            while (cont) {
                mul(b, b, sk1, lnw);
                mul(b, sk1, sk2, lnw);
                mul(sk0, sk2, sk1, lnw);
                sub(f, sk1, sk2, lnw);

                lnw = nw1;

                mul(b, sk2, sk1, lnw);
                mpdivd(sk1, new Chunk(3.0), sk2, lnw);

                lnw = nw2;

                add(b, sk2, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else cont = false;
            }
        }

        mul(a, b, sk0, lnw);

        int nw1 = lnw;
        lnw = Math.min((lnw << 1) - 2, nws) + 1;
        int nw2 = lnw;

        mul(sk0, sk0, sk1, lnw);
        mul(sk0, sk1, sk2, lnw);
        sub(a, sk2, sk1, lnw);

        lnw = nw1;

        mul(sk1, b, sk2, lnw);
        mpdivd(sk2, new Chunk(3.0), sk1, lnw);

        lnw = nw2;

        add(sk0, sk1, sk2, lnw);
        eq(sk2, b, lnw);
        round(b, nws);
    }

    static int compare(PreciseNumber a, PreciseNumber b, int lnw) {
        int i = 0;
        int ic = 0;
        int ia = (a.sign ? 1 : -1);

        if (a.number_words == 0.0) ia = 0;

        int ib = (b.sign ? 1 : -1);

        if (b.number_words == 0.0) ib = 0;

        int na = Math.min(a.number_words, lnw);
        int nb = Math.min(b.number_words, lnw);

        if (ia != ib)
            ic = (int)(fSign(1, ia - ib));
        else if (a.exponent != b.exponent)
            ic = (int)(ia * fSign(1, a.exponent - b.exponent));
        else {
            boolean sameMantissas = true;

            for (i = 0; i < Math.min(na, nb); i++) {
                if (a.mantissa[i] != b.mantissa[i]) {
                    ic = (int)(ia * fSign(1., a.mantissa[i] - b.mantissa[i]));
                    sameMantissas = false;
                    break;
                }
            }

            if (sameMantissas)
                ic = (na != nb) ? (int)(ia * fSign(1, na - nb)) : 0;
        }
        return ic;
    }

    static void mpdiv(PreciseNumber a, PreciseNumber b, PreciseNumber c, int lnw) {
        int i = 0;
        int j = 0;
        double rb = 0.0;
        double ss = 0.0;
        double t0 = 0.0;
        double t1 = 0.0;
        double t2 = 0.0;
        int na = Math.min(a.number_words, lnw);
        int nb = Math.min(b.number_words, lnw);

        if (na == 0) {
            zero(c);
            return;
        }

        if (nb == 1 && b.mantissa[0] == 1.0) {
            c.number_words = na;
            c.sign = !(a.sign ^ b.sign);
            c.exponent = a.exponent - b.exponent;

            for (i = 0; i < na; i++)
                c.mantissa[i] = a.mantissa[i];
            return;
        }

        if (nb == 0) throw new ArithmeticException("mpdiv: Divisor is zero.");

        double d[] = new double[lnw + 4];
        t0 = 1.6777216e7 * b.mantissa[0];

        if (nb >= 2) t0 += b.mantissa[1];
        if (nb >= 3) t0 += 5.9604644775390625e-8 * b.mantissa[2];
        if (nb >= 4) t0 += 3.552713678800501e-15 * b.mantissa[3];

        rb = 1.0 / t0;
        int md = Math.min(na + nb, lnw);
        d[0] = 0.0;

        for (i = 1; i <= na; i++) d[i] = a.mantissa[i - 1];
        for (i = na + 1; i < md + 4; i++) d[i] = 0.0;
        for (j = 2; j <= na + 1; j++) {
            t1 = 2.81474976710656e14 * d[j - 2] + 1.6777216e7 * d[j - 1] + d[j] + 5.9604644775390625e-8 * d[j + 1];
            t0 = (int)(rb * t1);
            int j3 = j - 3;
            int i2 = Math.min(nb, lnw + 2 - j3) + 2;
            int ij = i2 + j3;

            for (i = 2; i < i2; i++)
                d[i + j3] -= t0 * b.mantissa[i - 2];

            if (((j - 1) % 32) == 0) {
                for (i = j; i < ij; i++) {
                    t1 = d[i];
                    t2 = (int)(5.9604644775390625e-8 * t1);
                    d[i] = t1 - 1.6777216e7 * t2;
                    d[i - 1] += t2;
                }
            }

            d[j - 1] += 1.6777216e7 * d[j - 2];
            d[j - 2] = t0;
        }

        boolean stopped = false;

        for (j = na + 2; j <= lnw + 3; j++) {
            t1 = 2.81474976710656e14 * d[j - 2] + 1.6777216e7 * d[j - 1] + d[j];

            if (j <= lnw + 2) t1 = t1 + 5.9604644775390625e-8 * d[j + 1];

            t0 = (int)(rb * t1);
            int j3 = j - 3;
            int i2 = Math.min(nb, lnw + 2 - j3) + 2;
            int ij = i2 + j3;
            ss = 0.0;
            int i3 = 0;

            for (i = 2; i < i2; i++) {
                i3 = i + j3;
                d[i3] -= t0 * b.mantissa[i - 2];
                ss = ss + Math.abs(d[i3]);
            }

            if (((j - 1) % 32) == 0) {
                for (i = j; i < ij; i++) {
                    t1 = d[i];
                    t2 = (int)(5.9604644775390625e-8 * t1);
                    d[i] = t1 - 1.6777216e7 * t2;
                    d[i - 1] += t2;
                }
            }

            d[j - 1] += 1.6777216e7 * d[j - 2];
            d[j - 2] = t0;

            if (ss == 0.0) {
                stopped = true;
                break;
            }
            if (ij <= lnw + 1) d[ij + 2] = 0.0;
        }

        if (!stopped)
            j = lnw + 3;

        d[j - 1] = 0.0;
        int is = (d[0] == 0.0) ? 1 : 2;
        int nc = Math.min(j - 1, lnw);
        d[nc + 2] = 0.0;
        d[nc + 3] = 0.0;

        for (i = j; i >= 2; i--)
            d[i] = d[i - is];

        d[0] = fSign(nc, (!(a.sign ^ b.sign)) ? 1 : -1);
        d[1] = a.exponent - b.exponent + is - 2;

        mpnorm(d, c, lnw);
    }

    static void mpdivd(PreciseNumber a, Chunk b, PreciseNumber c, int lnw) {
        int j = 0;
        int k = 0;
        double bb = 0.0;
        double br = 0.0;
        double dd = 0.0;
        double t1 = 0.0;
        PreciseNumber f = new PreciseNumber(6, false);
        int na = Math.min(a.number_words, lnw);
        int ib = (int)(fSign(1.0, b.a));

        if (na == 0) {
            zero(c);
            return;
        }

        if (b.a == 0.0)
            throw new ArithmeticException("mpdivd: Divisor is zero");

        int n1 = b.n / 24;
        int n2 = b.n - 24 * n1;
        bb = Math.abs(b.a) * Math.pow(2.0, n2);

        if (bb >= 1.6777216e7) {
            for (k = 1; k <= 100; k++) {
                bb = 5.9604644775390625e-8 * bb;

                if (bb < 1.6777216e7) {
                    n1 += k;
                    break;
                }
            }
        } else if (bb < 1.0) {
            for (k = 1; k <= 100; k++) {
                bb = 1.6777216e7 * bb;

                if (bb >= 1.0) {
                    n1 -= k;
                    break;
                }
            }
        }

        if (bb != (int)(bb)) {
            dmc(new Chunk(fSign(bb, b.a), n1 * 24), f);
            mpdiv(a, f, c, lnw);
            return;
        }

        double d[] = new double[lnw + 4];
        br = 1.0 / bb;
        dd = a.mantissa[0];
        boolean skipJ = false;

        for (j = 2; j <= lnw + 3; j++) {
            t1 = (int)(br * dd);
            d[j] = t1;
            dd = 1.6777216e7 * (dd - t1 * bb);

            if (j <= na)
                dd = dd + a.mantissa[j - 1];
            else if (dd == 0.0) {
                skipJ = true;
                break;
            }
        }

        if (!skipJ) j = lnw + 3;

        int nc = Math.min(j - 1, lnw);
        d[0] = fSign(nc, (a.sign ? 1 : -1) * ib);
        d[1] = a.exponent - n1;

        if (j <= lnw + 2) d[j + 1] = 0.0;
        if (j <= lnw + 1) d[j + 2] = 0.0;

        mpnorm(d, c, lnw);
    }

    static void dmc(Chunk a, PreciseNumber b) {
        int i = 0;
        int k = 0;
        double aa = 0.0;

        if (a.a == 0.0) {
            zero(b);
            return;
        }

        int n1 = a.n / 24;
        int n2 = a.n - 24 * n1;
        aa = Math.abs(a.a) * Math.pow(2.0, n2);

        if (aa >= 1.6777216e7) {
            for (k = 1; k <= 100; k++) {
                aa = 5.9604644775390625e-8 * aa;
                if (aa < 1.6777216e7) {
                    n1 = n1 + k;
                    break;
                }
            }
        } else if (aa < 1.0) {
            for (k = 1; k <= 100; k++) {
                aa = 1.6777216e7 * aa;
                if (aa >= 1.0) {
                    n1 = n1 - k;
                    break;
                }
            }
        }

        b.exponent = n1;
        b.mantissa[0] = (float)((int)(aa));
        aa = 1.6777216e7 * (aa - b.mantissa[0]);
        b.mantissa[1] = (float)((int)(aa));
        aa = 1.6777216e7 * (aa - b.mantissa[1]);
        b.mantissa[2] = (float)((int)(aa));
        aa = 1.6777216e7 * (aa - b.mantissa[2]);
        b.mantissa[3] = (float)((int)(aa));
        b.mantissa[4] = 0;
        b.mantissa[5] = 0;

        for (i = 3; i >= 0; i--)
            if (b.mantissa[i] != 0.0) break;

        aa = i + 1;
        b.sign = (a.a >= 0);
        b.number_words = (int)(aa);
    }

    static void eq(PreciseNumber a, PreciseNumber b, int lnw) {
        int na = Math.min(a.number_words, lnw);

        if (na == 0) {
            zero(b);
            return;
        }

        b.number_words = na;
        b.sign = a.sign;
        b.exponent = a.exponent;

        for (int i = 0; i <= na; i++)
            b.mantissa[i] = a.mantissa[i];
    }

    static void infr(PreciseNumber a, PreciseNumber b, PreciseNumber c, int lnw) {
        int na = Math.min(a.number_words, lnw);
        int ma = a.exponent;

        if (na == 0) {
            zero(b);
            zero(c);
        }

        if (ma >= lnw - 1)
            throw new ArithmeticException("infr: Argument is too large -->" + a);

        int i = 0;
        int nb = Math.min(Math.max(ma + 1, 0), na);

        if (nb == 0)
            zero(b);
        else {
            b.number_words = nb;
            b.sign = a.sign;
            b.exponent = ma;
            b.mantissa[nb] = 0;
            b.mantissa[nb + 1] = 0;

            for (i = 0; i < nb; i++)
                b.mantissa[i] = a.mantissa[i];
        }

        int nc = na - nb;

        if (nc <= 0)
            zero(c);
        else {
            c.number_words = nc;
            c.sign = a.sign;
            c.exponent = ma - nb;
            c.mantissa[nc] = 0;
            c.mantissa[nc + 1] = 0;

            for (i = 0; i < nc; i++)
                c.mantissa[i] = a.mantissa[i + nb];
        }

        round(b, lnw);
        round(c, lnw);
    }

    static void mdc(PreciseNumber a, Chunk b) {
        double aa = 0.0;
        boolean isAZero = false;

        if (a.number_words == 0) {
            b.a = 0.0;
            b.n = 0;
            isAZero = true;
        }

        if (!isAZero) {
            int na = a.number_words;
            aa = a.mantissa[0];

            if (na >= 2) aa += 5.9604644775390625e-8 * a.mantissa[1];
            if (na >= 3) aa += 3.552713678800501e-15 * a.mantissa[2];
            if (na >= 4) aa += 5.9604644775390625e-8 * 3.552713678800501e-15 * a.mantissa[3];

            b.n = 24 * a.exponent;
            b.a = fSign(aa, a.sign ? 1.0 : -1.0);
        }
    }

    static void mul(PreciseNumber a, PreciseNumber b, PreciseNumber c, int lnw) {
        int i = 0;
        int j = 0;
        double t1 = 0.0;
        double t2 = 0.0;
        int na = Math.min(a.number_words, lnw);
        int nb = Math.min(b.number_words, lnw);

        if (na == 0 || nb == 0) {
            zero(c);
            return;
        }

        if (na == 1 && a.mantissa[0] == 1) {
            c.sign = !(a.sign ^ b.sign);
            c.number_words = nb;
            c.exponent = a.exponent + b.exponent;

            for (i = 0; i < nb; i++)
                c.mantissa[i] = b.mantissa[i];
            return;
        } else if (nb == 1 && b.mantissa[0] == 1.0) {
            c.sign = !(a.sign ^ b.sign);
            c.number_words = na;
            c.exponent = a.exponent + b.exponent;

            for (i = 0; i < na; i++)
                c.mantissa[i] = a.mantissa[i];
            return;
        }

        double d[] = new double[lnw + 4];
        int nc = Math.min(na + nb, lnw);
        double d2 = a.exponent + b.exponent;

        for (i = 0; i < nc + 4; i++)
            d[i] = 0.0;

        for (j = 3; j <= na + 2; j++) {
            t1 = a.mantissa[j - 3];
            int j3 = j - 3;
            int n2 = Math.min(nb + 2, lnw + 4 - j3);

            for (i = 2; i < n2; i++)
                d[i + j3] += t1 * b.mantissa[i - 2];

            if (((j - 2) % 32) == 0) {
                int i1 = Math.max(3, j - 32);
                int i2 = n2 + j3;

                for (i = i1 - 1; i < i2; i++) {
                    t1 = d[i];
                    t2 = (int)(5.9604644775390625e-8 * t1);
                    d[i] = t1 - 1.6777216e7 * t2;
                    d[i - 1] += t2;
                }
            }
        }

        if (d[1] != 0.0) {
            d2 += 1.0;

            for (i = nc + 3; i >= 2; i--)
                d[i] = d[i - 1];
        }

        d[0] = fSign(nc, (!(a.sign ^ b.sign)) ? 1.0 : -1.0);
        d[1] = d2;

        mpnorm(d, c, lnw);
    }

    static void muld(PreciseNumber a, Chunk b, PreciseNumber c, int lnw) {
        int i = 0;
        int k = 0;
        double bb = 0.0;
        PreciseNumber f = new PreciseNumber(6, false);
        int na = Math.min(a.number_words, lnw);
        int ib = (int)(fSign(1.0, b.a));

        if (na == 0 || b.a == 0.0) {
            zero(c);
            return;
        }

        int n1 = b.n / 24;
        int n2 = b.n - 24 * n1;
        bb = Math.abs(b.a) * Math.pow(2.0, n2);

        if (bb >= 1.6777216e7) {
            for (k = 1; k <= 100; k++) {
                bb = 5.9604644775390625e-8 * bb;

                if (bb < 1.6777216e7) {
                    n1 += k;
                    break;
                }
            }
        } else if (bb < 1.0) {
            for (k = 1; k <= 100; k++) {
                bb = 1.6777216e7 * bb;

                if (bb >= 1.0) {
                    n1 -= k;
                    break;
                }
            }
        }

        if (bb != (int)(bb)) {
            dmc(new Chunk(fSign(bb, b.a), n1 * 24), f);
            mul(f, a, c, lnw);
            return;
        }

        double d[] = new double[lnw + 4];

        for (i = 2; i < na + 2; i++)
            d[i] = bb * a.mantissa[i - 2];

        d[0] = fSign(na, (a.sign ? 1 : -1) * ib);
        d[1] = a.exponent + n1;
        d[na + 2] = 0.0;
        d[na + 3] = 0.0;

        mpnorm(d, c, lnw);
    }

    static void nint(PreciseNumber a, PreciseNumber b, int lnw) {
        int i = 0;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber s = new PreciseNumber(lnw + 2, false);
        int na = Math.min(a.number_words, lnw);

        if (na == 0) {
            zero(b);
            return;
        }

        if (a.exponent >= lnw)
            throw new ArithmeticException("nint: Argument is too large --> " + a);

        f.number_words = 1;
        f.sign = true;
        f.exponent = -1;
        f.mantissa[0] = (float)(0.5 * 1.6777216e7);
        f.mantissa[1] = 0;

        if (a.sign) add(a, f, s, lnw);
        else sub(a, f, s, lnw);

        int ic = s.sign ? 1 : -1;
        int nc = s.number_words;
        int mc = s.exponent;
        int nb = Math.min(Math.max(mc + 1, 0), nc);

        if (nb == 0)
            zero(b);
        else {
            b.number_words = nb;
            b.sign = (ic >= 0);
            b.exponent = mc;
            b.mantissa[nb] = 0;
            b.mantissa[nb + 1] = 0;

            for (i = 0; i < nb; i++)
                b.mantissa[i] = s.mantissa[i];
        }
    }

    private static void mpnorm(double d[], PreciseNumber a, int lnw) {
        final boolean risc = true;
        double t1 = 0.0;
        double t2 = 0.0;
        double t3 = 0.0;
        int i = 0;
        int ia = (int)(fSign(1.0, d[0]));
        int na = Math.min((int)(Math.abs(d[0])), lnw);

        if (na == 0) {
            zero(a);
            return;
        }

        int n4 = na + 4;
        double a2 = d[1];
        d[1] = 0.0;
        boolean needToNormalize = true;

        while (needToNormalize) {
            boolean breakLoop = false;

            if (!risc) {
                double s1 = 0.0;
                int k = 0;

                if (na > 8) {
                    for (k = 1; k <= 3; k++) {
                        s1 = 0.0;

                        for (i = 2; i < n4; i++) {
                            t2 = 5.9604644775390625e-8 * d[i];
                            t1 = (int)(t2);

                            if (t2 < 0.0 && t1 != t2) t1--;

                            d[i] -= t1 * 1.6777216e7;
                            d[i - 1] += t1;
                            s1 += Math.abs(t1);
                        }

                        if (s1 == 0.0) {
                            breakLoop = true;
                            break;
                        }
                    }
                }
            }

            if (!breakLoop) {
                t1 = 0;

                for (i = n4 - 1; i >= 2; i--) {
                    t3 = t1 + d[i];
                    t2 = 5.9604644775390625e-8 * (t3);
                    t1 = (int)(t2);

                    if (t2 < 0.0 && t1 != t2) t1--;

                    d[i] = t3 - t1 * 1.6777216e7;
                }
                d[1] += t1;
            }

            if (d[1] < 0.0) {
                ia = -ia;
                d[2] += 1.6777216e7 * d[1];
                d[1] = 0.0;

                for (i = 1; i < n4; i++)
                    d[i] = -d[i];
            } else if (d[1] > 0.0) {
                for (i = n4 - 2; i >= 1; i--)
                    a.mantissa[i - 1] = (float) d[i];

                na = Math.min(na + 1, lnw);

                a2++;

                needToNormalize = false;
            } else {
                for (i = 2; i < n4; i++)
                    a.mantissa[i - 2] = (float) d[i];
                needToNormalize = false;
            }
        }

        a.number_words = na;
        a.sign = (ia >= 0);
        a.exponent = (int)(a2);

        round(a, lnw);
    }

    static void mpnpwr(PreciseNumber a, int n, PreciseNumber b, int lnw) {
        int j = 0;
        double t1 = 0.0;
        int lnw3 = lnw + 3;
        PreciseNumber f1 = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(lnw3, false);
        PreciseNumber sk1 = new PreciseNumber(lnw3, false);
        int na = Math.min(a.number_words, lnw);

        if (na == 0) {
            if (n >= 0) {
                zero(b);
                return;
            } else throw new ArithmeticException("mpnpwr: Argument is zero and n is negative or zero --> " + a + "\n" + n);
        }

        int nws = lnw;

        lnw++;

        int nn = Math.abs(n);
        f1.sign = true;
        f1.number_words = 1;
        f1.exponent = 0;
        f1.mantissa[0] = 1;
        f1.mantissa[1] = 0;
        boolean skip = false;

        switch (nn) {
            case 0:
                eq(f1, b, lnw);
                return;

            case 1:
                eq(a, b, lnw);
                skip = true;
                break;

            case 2:
                mul(a, a, sk0, lnw);
                eq(sk0, b, lnw);
                skip = true;
                break;
        }

        if (!skip) {
            t1 = nn;
            int mn = (int)(CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14);

            eq(f1, b, lnw);
            eq(a, sk0, lnw);

            int kn = nn;

            for (j = 1; j <= mn; j++) {
                int kk = kn >> 1;

                if (kn != kk << 1) {
                    mul(b, sk0, sk1, lnw);
                    eq(sk1, b, lnw);
                }
                kn = kk;
                if (j < mn) {
                    mul(sk0, sk0, sk1, lnw);
                    eq(sk1, sk0, lnw);
                }
            }
        }

        if (n < 0) {
            mpdiv(f1, b, sk0, lnw);
            eq(sk0, b, lnw);
        }
        round(b, nws);
    }

    static void mpnrt(PreciseNumber a, int n, PreciseNumber b, int lnw) {
        int k = 0;
        double t2 = 0.0;
        double tn = 0.0;
        int lnw3 = lnw + 3;
        PreciseNumber f1 = new PreciseNumber(6, false);
        PreciseNumber f2 = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(lnw3, false);
        PreciseNumber sk1 = new PreciseNumber(lnw3, false);
        PreciseNumber sk2 = new PreciseNumber(lnw3, false);
        PreciseNumber sk3 = new PreciseNumber(lnw3, false);
        int na = Math.min(a.number_words, lnw);

        if (na == 0) {
            zero(b);
            return;
        }

        if (!a.sign) throw new ArithmeticException("mpnrt: Argument is negative -->" + a);
        if (n <= 0 || n > N30) throw new ArithmeticException("mpnrt: Improper value of n -->" + n);

        switch (n) {
            case 1:
                eq(a, b, lnw);
                return;
            case 2:
                mpsqrt(a, b, lnw);
                return;
            case 3:
                cbrt(a, b, lnw);
                return;
        }

        int nws = lnw;
        f1.number_words = 1;
        f1.sign = true;
        f1.exponent = 0;
        f1.mantissa[0] = 1;
        f1.mantissa[1] = 0;
        Chunk t1 = new Chunk();
        t1.a = lnw;
        int mq = (int)(CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14);

        sub(a, f1, sk0, lnw);

        if (sk0.number_words == 0) {
            eq(f1, b, lnw);
            return;
        }

        mdc(sk0, t1);

        int n2 = (int)(CL2 * Math.log(Math.abs(t1.a)));
        t1.a *= Math.pow(0.5, n2);
        t1.n += n2;

        if (t1.n <= -30) {
            t2 = n;
            n2 = (int)(CL2 * Math.log(t2) + 1.0 + 5.6843418860808015e-14);
            int n3 = -24 * lnw / t1.n;

            if (n3 < 1.25 * n2) {
                lnw++;

                mpdivd(sk0, new Chunk(t2), sk1, lnw);
                add(f1, sk1, sk2, lnw);

                k = 0;
                int temp = t1.n;
                t1.n = 0;

                do {
                    k++;

                    t1.a = 1 - k * n;
                    t2 = (k + 1) * n;

                    muld(sk1, t1, sk3, lnw);
                    mpdivd(sk3, new Chunk(t2), sk1, lnw);
                    mul(sk0, sk1, sk3, lnw);
                    eq(sk3, sk1, lnw);
                    add(sk1, sk2, sk3, lnw);
                    eq(sk3, sk2, lnw);
                }
                while (sk1.number_words != 0 && sk1.exponent >= -lnw);

                t1.n = temp;

                eq(sk2, b, lnw);
                mpdiv(f1, sk2, sk0, lnw);
                round(b, nws);
                return;
            }
        }

        tn = n;
        Chunk dpn = new Chunk(n, 0);

        mdc(a, t1);

        Chunk dp1 = new Chunk();
        dp1.n = (int)(-t1.n / tn);
        dp1.a = Math.exp(-1.0 / tn * (Math.log(t1.a) + (t1.n + tn * dp1.n) * ALT));

        dmc(dp1, b);
        dmc(dpn, f2);

        lnw = 3;
        int iq = 0;

        for (k = 2; k <= mq; k++) {
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            boolean loop = true;

            while (loop) {
                mpnpwr(b, n, sk0, lnw);
                mul(a, sk0, sk1, lnw);
                sub(f1, sk1, sk0, lnw);
                mul(b, sk0, sk1, lnw);
                mpdivd(sk1, new Chunk(tn), sk0, lnw);
                add(b, sk0, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else loop = false;
            }
        }

        mpdiv(f1, b, sk1, lnw);
        eq(sk1, b, lnw);
        round(b, nws);
    }

    static void rand(PreciseNumber a, int lnw) {
        int i = 0;
        double t1 = 0.0;
        double t2 = 0.0;

        synchronized(randMutex) {
            if (r30 == 0) {
                r30 = 1.0;
                t30 = 1.0;

                for (i = 1; i <= 30; i++) {
                    r30 = 0.50 * r30;
                    t30 = 2.0 * t30;
                }
            }

            a.number_words = lnw;
            a.sign = true;
            a.exponent = -1;

            for (i = 0; i <= lnw + 1; i++) {
                t1 = 78125 * sd;
                t2 = (int)(r30 * t1);
                sd = t1 - t30 * t2;
                a.mantissa[i] = (float)((int)(1.6777216e7 * r30 * sd));
            }
        }
        round(a, lnw);
    }

    static void round(PreciseNumber a, int lnw) {
        int i = 0;
        int a2 = a.exponent;
        a.exponent = 0;
        int na = Math.min(a.number_words, lnw);
        int n1 = na + 1;

        if (a.mantissa[0] == 0) {
            boolean allZero = true;

            for (i = 1; i <= n1; i++) {
                if (a.mantissa[i] != 0) {
                    allZero = false;
                    break;
                }
            }
            if (allZero) {
                zero(a);
                return;
            }

            int k = i;

            for (i = 0; i <= n1 - k; i++)
                a.mantissa[i] = a.mantissa[i + k];

            a2 -= k;
            na -= Math.max(k - 2, 0);
        }

        if (na == lnw && round >= 1) {
            if ((round == 1) && (a.mantissa[na] >= 0.5 * 1.6777216e7) || (round == 2) && (a.mantissa[na] >= 1))
                a.mantissa[na - 1]++;

            boolean loopBreak = false;

            for (i = na - 1; i >= 0; i--) {
                if (a.mantissa[i] < 1.6777216e7) {
                    loopBreak = true;
                    break;
                }
                a.mantissa[i] -= (float) 1.6777216e7;

                if (i != 0) a.mantissa[i - 1]++;
                else a.exponent++;
            }

            if (!loopBreak) {
                a.mantissa[0] = (float) a.exponent;
                na = 1;

                a2++;
            }
        }

        try {
            if (a.mantissa[na - 1] == 0) {
                boolean allZero = true;

                for (i = na - 1; i >= 0; i--) {
                    if (a.mantissa[i] != 0) {
                        allZero = false;
                        break;
                    }
                }
                if (allZero) {
                    zero(a);
                    return;
                }
                na = i + 1;
            }
        } catch (ArrayIndexOutOfBoundsException e) {}

        if (a2 < -2.e6) throw new ArithmeticException("round: Exponent underflow.");
        else if (a2 > 2.e6) throw new ArithmeticException("round: Exponent overflow.");

        if (a.mantissa[0] == 0)
            zero(a);
        else {
            a.number_words = na;
            a.exponent = a2;
            a.mantissa[na] = 0;
            a.mantissa[na + 1] = 0;
        }
    }

    static void mpsqrt(PreciseNumber a, PreciseNumber b, int lnw) {
        int k = 0;
        double t2 = 0.0;
        int lnw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(lnw3, false);
        PreciseNumber sk1 = new PreciseNumber(lnw3, false);
        PreciseNumber sk2 = new PreciseNumber(lnw3, false);
        int na = Math.min(a.number_words, lnw);

        if (na == 0) {
            zero(b);
            return;
        }

        if (!a.sign) throw new ArithmeticException("mpsqrt: Argument is negative --> " + a);

        int nws = lnw;
        Chunk t1 = new Chunk(lnw, 0);
        int mq = (int)(CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14); // *cast* 
        int iq = 0;

        mdc(a, t1);

        Chunk dp1 = new Chunk();
        dp1.n = -t1.n >> 1;
        t2 = Math.sqrt(t1.a * Math.pow(2.0, (t1.n + (dp1.n << 1))));
        dp1.a = 1.0 / t2;

        dmc(dp1, b);

        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;
        lnw = 3;
        iq = 0;
        int nw1 = 0;
        int nw2 = 0;

        for (k = 2; k <= mq - 1; k++) {
            nw1 = lnw;
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            nw2 = lnw;
            boolean stop = false;

            while (!stop) {
                mul(b, b, sk0, lnw);
                mul(a, sk0, sk1, lnw);
                sub(f, sk1, sk0, lnw);
                lnw = nw1;
                mul(b, sk0, sk1, lnw);
                muld(sk1, new Chunk(0.50), sk0, lnw);
                lnw = nw2;
                add(b, sk0, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else stop = true;
            }
        }

        mul(a, b, sk0, lnw);

        nw1 = lnw;
        lnw = Math.min((lnw << 1) - 2, nws) + 1;
        nw2 = lnw;

        mul(sk0, sk0, sk1, lnw);
        sub(a, sk1, sk2, lnw);

        lnw = nw1;

        mul(sk2, b, sk1, lnw);
        muld(sk1, new Chunk(0.50), sk2, lnw);

        lnw = nw2;

        add(sk0, sk2, sk1, lnw);
        eq(sk1, b, lnw);
        round(b, nws);
    }

    static void sub(PreciseNumber a, PreciseNumber b, PreciseNumber c, int lnw) {
        PreciseNumber bb = new PreciseNumber(0, false);
        bb.sign = !b.sign;
        bb.number_words = b.number_words;
        bb.exponent = b.exponent;
        bb.mantissa = b.mantissa;

        add(a, bb, c, lnw);

        bb.mantissa = null;
    }

    static void zero(PreciseNumber in ) { in .number_words = 0; in .sign = true; in .exponent = 0;
    }

    Chunk toDPE() {
        Chunk res = new Chunk();
        mdc(this, res);
        return res;
    }

    static void inp_complex(char a[], int n, PreciseNumber b, int lnw) {
        int i = 0;
        int j = 0;
        int is = 0;
        int id = 0;
        double bi = 0.0;
        char ai = 0;
        char ca[] = new char[81];
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        int nws = lnw++;
        int i1 = 0;
        int nn = 0;
        boolean caretFound = false;

        for (i = 0; i < n; i++) {
            ai = a[i];
            if (ai == '^') {
                caretFound = true;
                break;
            }
            if (ai == '.' || ai == '+' || ai == '-') break;
        }

        for (j = 0; j < 81; j++) ca[j] = '\0';

        if (caretFound) {
            int i2 = i - 1;

            if (i2 > 79) throw new NumberFormatException("inp_complex: Syntax error in literal string.");

            j = 0;

            for (i = 0; i <= i2; i++) {
                ai = a[i];

                if (ai == ' ') continue;
                else if (!Character.isDigit(ai)) throw new NumberFormatException("inp_complex: Syntax error in literal string.");

                ca[j++] = ai;
            }

            if (ca[0] != '1' || ca[1] != '0') throw new NumberFormatException("inp_complex: Syntax error in literal string.");

            i1 = i2 + 2;
            boolean exit = true;

            for (i = i1; i < n; i++) {
                ai = a[i];

                if (ai == 'x' || ai == '*') {
                    exit = false;
                    break;
                }
            }
            if (exit) throw new NumberFormatException("inp_complex: Syntax error in literal string.");

            i2 = i - 1;
            int l1 = i2 - i1;

            if (l1 > 79) throw new NumberFormatException("inp_complex: Syntax error in literal string.");

            id = 0;
            is = 1;
            j = 0;

            for (i = 0; i <= l1; i++) {
                ai = a[i + i1];

                if (ai == ' ' || ai == '+') continue;
                else if (ai == '-' && id == 0) {
                    id = 1;
                    is = -1;
                } else {
                    if (!Character.isDigit(ai)) throw new NumberFormatException("inp_complex: Syntax error in literal string.");

                    id = 1;
                    ca[j++] = ai;
                }
            }

            ca[j] = '\0';
            nn = Integer.parseInt(new String(ca, 0, j));
            nn = is * nn;
            i1 = i2 + 2;
        }

        boolean exit = true;

        for (i = i1; i < n; i++) {
            if (a[i] != ' ') {
                exit = false;
                break;
            }
        }

        if (exit) throw new NumberFormatException("inp_complex: Syntax error in literal string.");

        i1 = i;

        if (a[i1] == '+') {
            i1++;
            is = 1;
        } else if (a[i1] == '-') {
            i1 = i1 + 1;
            is = -1;
        } else is = 1;

        int ib = 0;
        id = 0;
        int ip = 0;

        zero(sk2);

        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        int it = 0;
        int mm = 0;
        boolean cont = true;

        while (cont) {
            ip = 0;

            for (mm = 0; mm < 6; mm++)
                ca[mm] = '0';

            for (i = i1; i < n; i++) {
                ai = a[i];
                if (ai == ' ');
                else if (ai == '.') {
                    if (ip != 0) throw new NumberFormatException("inp_complex: Syntax error in literal string.");
                    ip = id;
                } else if (!Character.isDigit(ai))
                    throw new NumberFormatException("inp_complex: Syntax error in literal string.");
                else {
                    id++;
                    ca[ib++] = ai;
                }
                if (ib == 6 || i == (n - 1) && ib != 0) {
                    if (it != 0) {
                        ca[ib] = '\0';
                        bi = Integer.parseInt(new String(ca, 0, ib));

                        muld(sk2, new Chunk(1e6), sk0, lnw);

                        if (bi != 0) {
                            f.number_words = 1;
                            f.sign = true;
                            f.mantissa[0] = (float) bi;
                        } else {
                            f.number_words = 0;
                            f.sign = true;
                        }

                        add(sk0, f, sk2, lnw);

                        for (mm = 0; mm < 6; mm++)
                            ca[mm] = '0';
                    }
                    if ((i + 1) != n) ib = 0;
                }
            }

            if (it == 0) {
                ib = 6 - ib;

                if (ib == 6) ib = 0;
                it = 1;
            } else cont = false;
        }

        if (is == -1) sk2.sign = !sk2.sign;
        if (ip == 0) ip = id;

        nn += ip - id;
        f.number_words = 1;
        f.sign = true;
        f.mantissa[0] = 10;

        mpnpwr(f, nn, sk0, lnw);
        mul(sk2, sk0, sk1, lnw);
        eq(sk1, b, lnw);
        round(b, nws);
    }

    static int mpoutc(PreciseNumber a, char b[], int lnw) {
        int i = 0;
        int j = 0;
        int k = 0;
        int nn = 0;
        int n = 0;
        double aa = 0.0;
        double t1 = 0.0;
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        int na = Math.min(a.number_words, lnw);

        lnw++;

        f.sign = true;
        f.number_words = 1;
        f.exponent = 0;
        f.mantissa[0] = 10;
        int nx = 0;

        if (na != 0) {
            aa = a.mantissa[0];

            if (na >= 2) aa += 5.9604644775390625e-8 * a.mantissa[1];
            if (na >= 3) aa += 3.552713678800501e-15 * a.mantissa[2];
            if (na >= 4) aa += 5.9604644775390625e-8 * 3.552713678800501e-15 * a.mantissa[3];

            t1 = AL2 * 24 * a.exponent + log10(aa);
            nx = (t1 >= 0.0) ? ((int) t1) : ((int)(t1 - 1.0));

            mpnpwr(f, nx, sk0, lnw);
            mpdiv(a, sk0, sk1, lnw);

            boolean cont = true;

            while (cont) {
                if (sk1.exponent < 0) {
                    nx = nx - 1;

                    muld(sk1, new Chunk(10.0), sk0, lnw);
                    eq(sk0, sk1, lnw);
                } else if (sk1.mantissa[0] >= 10) {
                    nx++;

                    mpdivd(sk1, new Chunk(10.0), sk0, lnw);
                    eq(sk0, sk1, lnw);
                } else cont = false;
            }
            sk1.sign = true;
        } else nx = 0;

        b[0] = '1';
        b[1] = '0';
        b[2] = ' ';
        b[3] = '^';
        char ca[] = String.valueOf(nx).toCharArray();
        int len = ca.length;
        int blank = 14 - len;

        for (i = 4; i < blank; i++) b[i] = ' ';
        for (i = 0; i < len; i++) b[blank + i] = ca[i];

        b[14] = ' ';
        b[15] = 'x';
        b[16] = ' ';
        b[17] = (a.sign == false) ? '-' : ' ';
        nn = (na != 0) ? ((int) sk1.mantissa[0]) : 0;
        ca = String.valueOf(nn).toCharArray();
        b[18] = ca[0];
        b[19] = '.';
        int ix = 20;

        if (na == 0) {
            b[ix] = '\0';
            return ix;
        }

        f.mantissa[0] = (float) nn;

        sub(sk1, f, sk0, lnw);

        if (sk0.number_words == 0) {
            b[ix] = '\0';
            return ix;
        }

        muld(sk0, new Chunk(1e6), sk1, lnw);

        int nl = (int)(Math.max(lnw * log10(1.6777216e7) / 6.0 - 1.0, 1.0));
        boolean skip = false;

        for (j = 1; j <= nl; j++) {
            if (sk1.exponent == 0.0) {
                nn = (int)(sk1.mantissa[0]);
                f.number_words = 1;
                f.sign = true;
                f.mantissa[0] = (float) nn;
            } else {
                f.number_words = 0;
                f.sign = true;
                nn = 0;
            }

            ca = String.valueOf(nn).toCharArray();

            for (i = 0; i < 6 - ca.length; i++)
                b[i + ix] = '0';

            k = 0;

            for (; i < 6; i++) b[i + ix] = ca[k++];

            ix += 6;

            sub(sk1, f, sk0, lnw);
            muld(sk0, new Chunk(1e6), sk1, lnw);

            if (sk1.number_words == 0) {
                skip = true;
                break;
            }
        }

        if (!skip) j = nl + 1;

        int l = --ix;

        if (b[l] == '0' || (j > nl && b[l - 1] == '0' && b[l - 2] == '0' && b[l - 3] == '0')) {
            b[l] = '\0';
            boolean loopbreak = false;

            for (i = l - 1; i >= 20; i--) {
                if (b[i] != '0') {
                    ix = i;
                    loopbreak = true;
                    break;
                }
                b[i] = '\0';
            }

            if (!loopbreak)
                ix = 20;
        } else if (j > nl && b[l - 1] == '9' && b[l - 2] == '9' && b[l - 3] == '9') {
            b[l] = '\0';
            skip = false;

            for (i = l - 1; i >= 20; i--) {
                if (b[i] != '9') {
                    skip = true;
                    break;
                }
                b[i] = '\0';
            }

            if (!skip) {
                ix = 20;

                if (b[18] == '9') {
                    b[18] = '1';
                    ca = String.valueOf(nx + 1).toCharArray();
                    k = 0;

                    for (i = 0; i < 10 - ca.length; i++) b[i + 4] = ' ';
                    for (; i < 10; i++) b[i + 4] = ca[k++];
                } else {
                    ca[0] = b[18];
                    ca[1] = '\0';
                    nn = Integer.parseInt(new String(ca, 0, 1));
                    ca = String.valueOf(nn + 1).toCharArray();
                    b[18] = ca[0];
                }
            } else {
                ca[0] = b[i];
                ca[1] = '\0';
                nn = Integer.parseInt(new String(ca, 0, 1));
                ca = String.valueOf(nn + 1).toCharArray();
                b[i] = ca[0];
                ix = i;
            }
        }

        n = ix;
        b[++n] = '\0';

        return n;
    }

    static void dexc(char a[], int l, PreciseNumber b, int lnw) {
        int i = 0;
        boolean foundExponent = false;

        for (i = 0; i < l; i++) {
            if (a[i] == 'D' || a[i] == 'E' || a[i] == 'd' || a[i] == 'e') {
                foundExponent = true;
                break;
            }
        }
        if (!foundExponent) {
            inp_complex(a, l, b, lnw);
            return;
        }

        char c[] = new char[precision_digits + 101];
        int i1 = i + 1;
        int l1 = i;
        int l2 = l - i1;
        c[0] = '1';
        c[1] = '0';
        c[2] = '^';

        for (i = 0; i < l2; i++)
            c[i + 3] = a[i + i1];

        c[l2 + 3] = 'x';

        for (i = 0; i < l1; i++)
            c[i + l2 + 4] = a[i];

        c[i + l2 + 4] = '\0';

        inp_complex(c, l1 + l2 + 4, b, lnw);
    }

    static int mpouts(PreciseNumber a, int la, char[] cs, int lnw) {
        lnw = Math.min(lnw, (int)(la / log10(1.6777216e7) + 2.0));
        return Math.min((precision_digits < 10000) ? mpoutc(a, cs, lnw) : outx(a, cs, lnw), la + 20) + 1;
    }

    static void mpang(PreciseNumber x, PreciseNumber y, PreciseNumber pi, PreciseNumber a, int lnw) {
        int k = 0;
        int nw3 = lnw + 3;
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        PreciseNumber sk3 = new PreciseNumber(nw3, false);
        PreciseNumber sk4 = new PreciseNumber(nw3, false);
        int ix = x.sign ? 1 : -1;
        int nx = Math.min(x.number_words, lnw);
        int iy = y.sign ? 1 : -1;
        int ny = Math.min(y.number_words, lnw);

        if (nx == 0 && ny == 0)
            throw new ArithmeticException("mpang: Both arguments are zero.");

        Chunk t1 = new Chunk();

        mdc(pi, t1);

        if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15)
            throw new ArithmeticException("mpang: PI must be precomputed");

        if (nx == 0) {
            muld(pi, new Chunk((iy > 0) ? 0.5 : -0.5), a, lnw);
            return;
        } else if (ny == 0) {
            if (ix > 0) zero(a);
            else eq(pi, a, lnw);
            return;
        }

        int nws = lnw;

        lnw++;

        t1.a = nws;
        int mq = (int)(CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14);

        mul(x, x, sk0, lnw);
        mul(y, y, sk1, lnw);
        add(sk0, sk1, sk2, lnw);
        mpsqrt(sk2, sk3, lnw);
        mpdiv(x, sk3, sk1, lnw);
        mpdiv(y, sk3, sk2, lnw);
        mdc(sk1, t1);

        Chunk t2 = new Chunk();

        mdc(sk2, t2);

        t1.n = Math.max(t1.n, -66);
        t2.n = Math.max(t2.n, -66);
        t1.a = t1.a * Math.pow(2.0, t1.n);
        t2.a = t2.a * Math.pow(2.0, t2.n);
        Chunk t3 = new Chunk();
        t3.a = Math.atan2(t2.a, t1.a);

        dmc(t3, a);

        int kk;

        if (Math.abs(t1.a) <= Math.abs(t2.a)) {
            kk = 1;
            eq(sk1, sk0, lnw);
        } else {
            kk = 2;
            eq(sk2, sk0, lnw);
        }

        lnw = 3;
        int iq = 0;

        for (k = 2; k <= mq; k++) {
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            boolean cont = true;

            while (cont) {
                ssn_complex(a, pi, sk1, sk2, lnw);
                if (kk == 1) {
                    sub(sk0, sk1, sk3, lnw);
                    mpdiv(sk3, sk2, sk4, lnw);
                    sub(a, sk4, sk1, lnw);
                } else {
                    sub(sk0, sk2, sk3, lnw);
                    mpdiv(sk3, sk1, sk4, lnw);
                    add(a, sk4, sk1, lnw);
                }
                eq(sk1, a, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else cont = false;
            }
        }
        round(a, nws);
    }

    static void mpcssh(PreciseNumber a, PreciseNumber al2, PreciseNumber x, PreciseNumber y, int lnw) {
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        PreciseNumber sk3 = new PreciseNumber(nw3, false);
        int nws = lnw;

        lnw++;

        f.sign = true;
        f.number_words = 1;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;

        mpexp(a, al2, sk0, lnw);
        mpdiv(f, sk0, sk1, lnw);
        add(sk0, sk1, sk2, lnw);
        muld(sk2, new Chunk(0.5), sk3, lnw);
        eq(sk3, x, lnw);
        sub(sk0, sk1, sk2, lnw);
        muld(sk2, new Chunk(0.5), sk3, lnw);
        eq(sk3, y, lnw);
        round(x, nws);
        round(y, nws);
    }

    static void ssn_complex(PreciseNumber a, PreciseNumber pi, PreciseNumber x, PreciseNumber y, int lnw) {
        double t2 = 0.0;
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        PreciseNumber sk3 = new PreciseNumber(nw3, false);
        PreciseNumber sk4 = new PreciseNumber(nw3, false);
        PreciseNumber sk5 = new PreciseNumber(nw3, false);
        PreciseNumber sk6 = new PreciseNumber(nw3, false);
        int na = Math.min(a.number_words, lnw);
        int l1 = 0;

        if (na == 0) {
            x.sign = true;
            x.number_words = 1;
            x.exponent = 0;
            x.mantissa[0] = 1;

            zero(y);

            l1 = 0;
            return;
        }

        Chunk t1 = new Chunk();

        mdc(pi, t1);

        if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15)
            throw new ArithmeticException("mpccsn: pi must be precomputed.");

        int nws = lnw;

        lnw++;

        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;

        muld(pi, new Chunk(2.0), sk0, lnw);
        mpdiv(a, sk0, sk1, lnw);
        nint(sk1, sk2, lnw);
        sub(sk1, sk2, sk3, lnw);
        mdc(sk3, t1);

        int ka = 0;
        int kb = 0;

        if (t1.n >= -24) {
            t1.a *= Math.pow(2.0, t1.n);
            t2 = 4.0 * t1.a;
            ka = (int)(nint(t2));
            kb = (int)(nint(8.0 * (t2 - ka)));
        } else {
            ka = 0;
            kb = 0;
        }

        t1.a = ((ka << 3) + kb) / 32.0;
        t1.n = 0;

        dmc(t1, sk1);
        sub(sk3, sk1, sk2, lnw);
        mul(sk0, sk2, sk1, lnw);

        if (sk1.number_words == 0) {
            zero(sk0);
            l1 = 0;
        } else {
            eq(sk1, sk0, lnw);
            mul(sk0, sk0, sk2, lnw);

            l1 = 0;

            do {
                l1 = l1 + 1;

                if (l1 == 10000)
                    throw new ArithmeticException("ssn_complex: Iteration limit exceeded.");

                t2 = -(2.0 * l1) * (2.0 * l1 + 1.0);

                mul(sk2, sk1, sk3, lnw);
                mpdivd(sk3, new Chunk(t2), sk1, lnw);
                add(sk1, sk0, sk3, lnw);
                eq(sk3, sk0, lnw);
            }
            while (sk1.number_words != 0 && sk1.exponent >= sk0.exponent - lnw);
        }

        eq(sk0, sk1, lnw);
        mul(sk0, sk0, sk2, lnw);
        sub(f, sk2, sk3, lnw);
        mpsqrt(sk3, sk0, lnw);

        int kc = 0;
        kc = Math.abs(kb);
        f.mantissa[0] = 2;

        if (kc == 0) {
            sk2.sign = true;
            sk2.number_words = 1;
            sk2.exponent = 0;
            sk2.mantissa[0] = 1;

            zero(sk3);
        } else {
            switch (kc) {
                case 1:
                    mpsqrt(f, sk4, lnw);
                    add(f, sk4, sk5, lnw);
                    mpsqrt(sk5, sk4, lnw);
                    break;

                case 2:
                    mpsqrt(f, sk4, lnw);
                    break;

                case 3:
                    mpsqrt(f, sk4, lnw);
                    sub(f, sk4, sk5, lnw);
                    mpsqrt(sk5, sk4, lnw);
                    break;

                case 4:
                    zero(sk4);
                    break;
            }

            add(f, sk4, sk5, lnw);
            mpsqrt(sk5, sk3, lnw);
            muld(sk3, new Chunk(0.5), sk2, lnw);
            sub(f, sk4, sk5, lnw);
            mpsqrt(sk5, sk4, lnw);
            muld(sk4, new Chunk(0.5), sk3, lnw);
        }

        if (kb < 0) sk3.sign = !sk3.sign;

        mul(sk0, sk2, sk4, lnw);
        mul(sk1, sk3, sk5, lnw);
        sub(sk4, sk5, sk6, lnw);
        mul(sk1, sk2, sk4, lnw);
        mul(sk0, sk3, sk5, lnw);
        add(sk4, sk5, sk1, lnw);
        eq(sk6, sk0, lnw);

        switch (ka) {
            case 0:
                eq(sk0, x, lnw);
                eq(sk1, y, lnw);
                break;

            case 1:
                eq(sk1, x, lnw);
                x.sign = !x.sign;
                eq(sk0, y, lnw);
                break;

            case -1:
                eq(sk1, x, lnw);
                eq(sk0, y, lnw);
                y.sign = !y.sign;
                break;

            case 2:
            case -2:
                eq(sk0, x, lnw);
                x.sign = !x.sign;
                eq(sk1, y, lnw);
                y.sign = !y.sign;
                break;
        }

        round(x, nws);
        round(y, nws);
    }

    static void mpexp(PreciseNumber a, PreciseNumber al2, PreciseNumber b, int lnw) {
        int i = 0;
        int l1 = 0;
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        PreciseNumber sk3 = new PreciseNumber(nw3, false);
        Chunk t1 = new Chunk();

        mdc(a, t1);

        t1.a = t1.value();
        Chunk t2 = new Chunk();

        if (Math.abs(t1.a - ALT) > 5.9604644775390625e-8) {
            mdc(al2, t2);

            if (t2.n != -24 || Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15)
                throw new ArithmeticException("mpexp: LOG (2) must be precomputed.");
        }

        if (t1.a >= 1e9) {
            if (t1.a > 0.0)
                throw new ArithmeticException("MPEXP: Argument is too large --> " + t1.a + " x 10 ^" + t1.n);
            else {
                zero(b);
                l1 = 0;
                return;
            }
        }

        int nws = lnw;

        lnw++;

        f.sign = true;
        f.number_words = 1;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;
        int nz = 0;

        if (Math.abs(t1.a - ALT) > 5.9604644775390625e-8) {
            mpdiv(a, al2, sk0, lnw);
            nint(sk0, sk1, lnw);
            mdc(sk1, t1);

            nz = (int)(t1.value() + fSign(5.6843418860808015e-14, t1.a));

            mul(al2, sk1, sk2, lnw);
            sub(a, sk2, sk0, lnw);
        } else {
            eq(a, sk0, lnw);

            nz = 0;
        }

        double tl = sk0.exponent - lnw;
        boolean skip = false;

        if (sk0.number_words == 0) {
            sk0.number_words = 1;
            sk0.sign = true;
            sk0.exponent = 0;
            l1 = 0;
            skip = true;
        }

        if (!skip) {
            mpdivd(sk0, new Chunk(1.0, 8), sk1, lnw);
            eq(f, sk2, lnw);
            eq(f, sk3, lnw);

            l1 = 0;
            t2.n = 0;

            do {
                l1 = l1 + 1;

                if (l1 == 10000)
                    throw new ArithmeticException("mpexp: Iteration limit exceeded.");

                t2.a = l1;

                mul(sk2, sk1, sk0, lnw);
                mpdivd(sk0, t2, sk2, lnw);
                add(sk3, sk2, sk0, lnw);
                eq(sk0, sk3, lnw);
            }
            while (sk2.number_words != 0.0 && sk2.exponent >= tl);

            for (i = 1; i <= 8; i++) {
                mul(sk0, sk0, sk1, lnw);
                eq(sk1, sk0, lnw);
            }
        }

        muld(sk0, new Chunk(1.0, nz), sk1, lnw);
        eq(sk1, b, lnw);
        round(b, nws);
    }

    static void log(PreciseNumber a, PreciseNumber al2, PreciseNumber b, int lnw) {
        int k = 0;
        int nw3 = lnw + 3;
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        int na = Math.min(a.number_words, lnw);

        if (a.sign == false || na == 0)
            throw new ArithmeticException("log: Argument is less than or equal to zero -->" + a);

        Chunk t1 = new Chunk();
        Chunk t2 = new Chunk();

        mdc(a, t1);

        if (Math.abs(t1.a - 2.0) > 1e-3 || t1.n != 0) {
            mdc(al2, t2);

            if (t2.n != -24 || Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15)
                throw new ArithmeticException("log: LOG (2) must be precomputed.");
        }

        if (a.number_words == 1 && a.sign && a.exponent == 0 && a.mantissa[0] == 1.0) {
            b.number_words = 0;
            b.exponent = 0;
            b.sign = true;
            return;
        }

        int nws = lnw;
        t2.a = nws;
        int mq = (int)(CL2 * Math.log(t2.a) + 1.0 - 5.6843418860808015e-14);
        t1.a = Math.log(t1.a) + t1.n * ALT;
        t1.n = 0;

        dmc(t1, b);

        lnw = 3;
        int iq = 0;

        for (k = 2; k <= mq; k++) {
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            boolean cont = true;

            while (cont) {
                mpexp(b, al2, sk0, lnw);
                sub(a, sk0, sk1, lnw);
                mpdiv(sk1, sk0, sk2, lnw);
                add(b, sk2, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else cont = false;
            }
        }

        round(b, nws);
    }

    static void mppi(PreciseNumber pi, int lnw) {
        int k = 0;
        double t1 = 0.0;
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        PreciseNumber sk3 = new PreciseNumber(nw3, false);
        PreciseNumber sk4 = new PreciseNumber(nw3, false);
        int nws = lnw;

        lnw++;

        t1 = nws * log10(1.6777216e7);
        int mq = (int)(CL2 * (Math.log(t1) - 1.0) + 1.0);
        sk0.number_words = 1;
        sk0.sign = true;
        sk0.exponent = 0;
        sk0.mantissa[0] = 1;
        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        f.mantissa[0] = 2;
        f.mantissa[1] = 0;

        mpsqrt(f, sk2, lnw);
        muld(sk2, new Chunk(0.50), sk1, lnw);

        f.exponent = -1;
        f.mantissa[0] = (float)(0.50 * 1.6777216e7);

        sub(sk2, f, sk4, lnw);

        for (k = 1; k <= mq; k++) {
            add(sk0, sk1, sk2, lnw);
            mul(sk0, sk1, sk3, lnw);
            mpsqrt(sk3, sk1, lnw);
            muld(sk2, new Chunk(0.50), sk0, lnw);
            sub(sk0, sk1, sk2, lnw);
            mul(sk2, sk2, sk3, lnw);

            t1 = Math.pow(2.0, k);

            muld(sk3, new Chunk(t1), sk2, lnw);
            sub(sk4, sk2, sk3, lnw);
            eq(sk3, sk4, lnw);
        }

        add(sk0, sk1, sk2, lnw);
        mul(sk2, sk2, sk3, lnw);
        mpdiv(sk3, sk4, sk2, lnw);
        eq(sk2, pi, lnw);
        round(pi, nws);
    }

    static void mpfftcr(int is, int m, int n, Complex x[], double y[]) {
        int k = 0;
        final Complex pointFive = new Complex(0.5);
        final Complex zeroOne = new Complex(0.0, 1.0);
        int mx = (int) uu1[0].real();

        if ((is != 1 && is != -1) || m < 3 || m > mx)
            throw new ArithmeticException("mpfftcr: Either the UU arrays have not been initialized or one of the input parameters is invalid: " + is + "\t" + m + "\t" + mx);

        Complex dc1[] = new Complex[n >> 1];
        Complex a1 = null;
        Complex a2 = null;
        Complex x1 = null;
        Complex x2 = null;
        int n1 = (int)(Math.pow(2, (m >> 1)));
        int n2 = n >> 1;
        int n4 = n >> 2;
        dc1[0] = pointFive.multiply(new Complex((x[0].add(x[n2])).real(), (x[0].subtract(x[n2])).real()));
        dc1[n4] = (is == 1) ? x[n4].conjg() : ((Complex) x[n4].clone());
        int ku = n2;

        if (is == 1) {
            for (k = 1; k < n4; k++) {
                x1 = x[k];
                x2 = x[n2 - k].conjg();
                a1 = x1.add(x2);
                a2 = zeroOne.multiply(uu1[k + ku]).multiply(x1.subtract(x2));
                dc1[k] = pointFive.multiply(a1.add(a2));
                dc1[n2 - k] = pointFive.multiply((a1.subtract(a2)).conjg());
            }
        } else {
            for (k = 1; k < n4; k++) {
                x1 = x[k];
                x2 = (x[n2 - k]).conjg();
                a1 = x1.add(x2);
                a2 = zeroOne.multiply(uu1[k + ku].conjg()).multiply(x1.subtract(x2));
                dc1[k] = pointFive.multiply(a1.add(a2));
                dc1[n2 - k] = pointFive.multiply((a1.subtract(a2)).conjg());
            }
        }

        mpfft1(is, m - 1, n1, n2 / n1, dc1, x);

        for (k = 0; k < n >> 1; k++) {
            y[k << 1] = dc1[k].real();
            y[(k << 1) + 1] = dc1[k].aimag();
        }
    }

    static void mpfftrc(int is, int m, int n, double x[], Complex y[]) {
        int k = 0;
        int mx = (int) uu1[0].real();

        if ((is != 1 && is != -1) || m < 3 || m > mx)
            throw new ArithmeticException("mpfftrc: Either the UU arrays have not been initialized or one of the input parameters is invalid: " + is + "\t" + m + "\t" + mx);

        Complex dc1[] = new Complex[n >> 1], a1, a2, z1, z2;
        int n1 = (int) Math.pow(2, (m >> 1));
        int n2 = n >> 1;
        int n4 = n >> 2;

        for (k = 0; k < n2; k++)
            dc1[k] = new Complex(x[k << 1], x[(k << 1) + 1]);

        mpfft1(is, m - 1, n1, n2 / n1, dc1, y);

        y[0] = new Complex(2.0 * (dc1[0].real() + dc1[0].aimag()), 0.0);
        y[n4] = ((is == 1) ? (dc1[n4]) : ((dc1[n4].conjg()))).multiply(new Complex(2.0));
        y[n2] = new Complex(2.0 * (dc1[0].real() - dc1[0].aimag()), 0.0);
        int ku = n2;
        final Complex zeroMinOne = new Complex(0.0, -1.0);

        if (is == 1) {
            for (k = 1; k < n4; k++) {
                z1 = dc1[k];
                z2 = dc1[n2 - k].conjg();
                a1 = z1.add(z2);
                a2 = zeroMinOne.multiply(uu1[k + ku]).multiply(z1.subtract(z2));
                y[k] = a1.add(a2);
                y[n2 - k] = a1.subtract(a2).conjg();
            }
        } else {
            for (k = 1; k < n4; k++) {
                z1 = dc1[k];
                z2 = dc1[n2 - k].conjg();
                a1 = z1.add(z2);
                a2 = zeroMinOne.multiply(uu1[k + ku].conjg()).multiply(z1.subtract(z2));
                y[k] = a1.add(a2);
                y[n2 - k] = a1.subtract(a2).conjg();
            }
        }
    }

    static void mpfft1(int is, int m, int n1, int n2, Complex x[], Complex y[]) {
        int i = 0;
        int j = 0;
        int k = 0;
        Complex z1[][] = new Complex[18][n1];
        Complex z2[][] = new Complex[18][n1];
        int yrow = n2 + 2;
        int m1 = (m + 1) >> 1;
        int m2 = m - m1;
        int nr1 = Math.min(n1, 16);
        int nr2 = Math.min(n2, 16);
        int ku = (int) uu2[m - 1].real();

        for (i = 0; i < n1; i += nr1) {
            for (k = 0; k < nr1; k++) {
                for (j = 0; j < n2; j++)
                    z1[k][j] = x[j * n1 + i + k];
            }

            mpfft2(is, nr1, m2, n2, z1, z2);

            int iu = i + ku - n1 - 1;

            if (is == 1) {
                for (k = 0; k < nr1; k++) {
                    for (j = 0; j < n2; j++)
                        y[(i + k) * yrow + j] = uu2[iu + k + (j + 1) * n1].multiply(z1[k][j]);
                }
            } else {
                for (k = 0; k < nr1; k++) {
                    for (j = 0; j < n2; j++)
                        y[(i + k) * yrow + j] = uu2[iu + k + (j + 1) * n1].conjg().multiply(z1[k][j]);
                }
            }
        }

        for (i = 0; i < n2; i += nr2) {

            for (k = 0; k < nr2; k++) {
                for (j = 0; j < n1; j++)
                    z2[k][j] = y[j * yrow + i + k];
            }

            mpfft2(is, nr2, m1, n1, z2, z1);

            if ((m % 2) == 0) {
                for (k = 0; k < nr2; k++) {
                    for (j = 0; j < n1; j++)
                        x[i + k + j * n1] = z2[k][j];
                }
            } else {
                int j2 = 0;

                for (j = 0; j < n1 >> 1; j++) {
                    j2 = (j << 1);

                    for (k = 0; k < nr2; k++) {
                        x[i + k + j * n1] = z2[k][j2];
                        x[i + k + n2 + n1 * j] = z2[k][j2 + 1];
                    }
                }
            }
        }
    }

    static void mpfft2(int is, int ns, int m, int n, Complex x[][], Complex y[][]) {
        int l = 0;
        int j = 0;
        int i = 0;

        for (l = 1; l <= m; l += 2) {
            mpfft3(is, l, ns, m, n, x, y);

            if (l == m) {
                for (i = 0; i < ns; i++) {
                    for (j = 0; j < n; j++)
                        x[i][j] = y[i][j];
                }
                return;
            }

            mpfft3(is, l + 1, ns, m, n, y, x);
        }
    }

    static void mpfft3(int is, int l, int ns, int m, int n, Complex x[][], Complex y[][]) {
        Complex u1 = null;
        Complex x1 = null;
        Complex x2 = null;
        int i = 0;
        int j = 0;
        int k = 0;
        int n1 = n >> 1;
        int lk = (int) Math.pow(2, (l - 1));
        int li = (int) Math.pow(2, (m - l));
        int lj = (lk << 1);
        int ku = li;
        int i11 = 0;
        int i12 = 0;
        int i21 = 0;
        int i22 = 0;

        for (i = 0; i <= li - 1; i++) {
            i11 = i * lk + 1;
            i12 = i11 + n1;
            i21 = i * lj + 1;
            i22 = i21 + lk;
            u1 = (is == 1) ? uu1[i + ku] : (uu1[i + ku].conjg());

            for (k = -1; k < lk - 1; k++) {
                for (j = 0; j < ns; j++) {
                    x1 = x[j][i11 + k];
                    x2 = x[j][i12 + k];
                    y[j][i21 + k] = x1.add(x2);
                    y[j][i22 + k] = u1.multiply(x1.subtract(x2));
                }
            }
        }
    }

    static void mplconv(int iq, int n, int nsq, double a[], double b[], double c[]) {
        int i = 0;
        int j = 0;
        int k = 0;
        double an = 0.0;
        double t1 = 0.0;
        double t2 = 0.0;
        int ncr1 = (int) Math.pow(2, (pointer - 1));
        int n1 = 0;
        int n2 = 0;

        if (n < ncr1) {
            switch (iq) {
                case 1:
                    for (k = 0; k < (n << 1); k++) {
                        t1 = 0.0;
                        n1 = Math.max(k - n + 2, 1);
                        n2 = Math.min(k + 1, n);

                        for (j = n1 - 1; j < n2; j++)
                            t1 += a[j] * a[k - j];

                        c[k] = t1;
                    }
                    break;

                case 2:
                    for (k = 0; k < (n << 1); k++) {
                        t1 = 0.0;
                        n1 = Math.max(k - n + 2, 1);
                        n2 = Math.min(k + 1, n);

                        for (j = n1 - 1; j < n2; j++)
                            t1 += a[j] * b[k - j];

                        c[k] = t1;
                    }
                    break;

                case -1:
                    for (k = 0; k < n - 1; k++)
                        c[k] = 0.0;
                    for (k = n - 1; k < (n << 1); k++) {
                        t1 = 0.0;
                        n1 = k - n + 2;
                        n2 = n;

                        for (j = n1 - 1; j < n2; j++)
                            t1 += a[j] * a[k - j];

                        c[k] = t1;
                    }
                    break;

                case -2:
                    for (k = 0; k < n - 1; k++)
                        c[k] = 0.0;
                    for (k = n - 1; k < (n << 1); k++) {
                        t1 = 0.0;
                        n1 = k - n + 2;
                        n2 = n;

                        for (j = n1 - 1; j < n2; j++)
                            t1 += a[j] * b[k - j];

                        c[k] = t1;
                    }
                    break;
            }
            return;
        }

        double d1[] = new double[3 * n + 2];
        double d2[] = new double[3 * n + 2];
        double d3[] = new double[3 * n + 2];
        Complex dc1[] = new Complex[3 * (n >> 1) + (nsq << 1) + 3];
        Complex dc2[] = new Complex[3 * (n >> 1) + (nsq << 1) + 3];
        t1 = 0.75 * n;
        int m1 = (int)(CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14);
        n1 = (int)(Math.pow(2, m1));
        int m2 = m1 + 1;
        n2 = n1 << 1;
        int n4 = n2 << 1;
        int nm = Math.min((n << 1), n2);

        if (Math.abs(iq) == 1) {
            for (i = 0; i < n; i++) d1[i] = a[i];
            for (i = n; i < n2; i++) d1[i] = 0.0;

            mpfftrc(1, m2, n2, d1, dc1);

            for (i = 0; i < n1 + 1; i++)
                dc1[i] = dc1[i].multiply(dc1[i]);
        } else {
            for (i = 0; i < n; i++) {
                d1[i] = a[i];
                d2[i] = b[i];
            }

            for (i = n; i < n2; i++) {
                d1[i] = 0.0;
                d2[i] = 0.0;
            }

            mpfftrc(1, m2, n2, d1, dc1);
            mpfftrc(1, m2, n2, d2, dc2);

            for (i = 0; i <= n1; i++)
                dc1[i] = dc1[i].multiply(dc2[i]);
        }

        mpfftcr(-1, m2, n2, dc1, d3);

        an = 1.0 / n4;

        for (i = 0; i < nm; i++) {
            t1 = an * d3[i];
            t2 = nint(t1);
            c[i] = t2;
        }

        boolean skip = true;

        if (!skip) {
            int i1 = 0;
            t1 = 0.0;

            for (i = 0; i < nm; i++) {
                if (d1[i] > t1) {
                    i1 = i;
                    t1 = d1[i];
                }
            }

            if (t1 > 0.438) {
                t2 = an * d1[i1];
                int i2 = (int)(CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14);
                int i3 = (int)(CL2 * Math.log(t2) + 1.0 + 5.6843418860808015e-14);
                int i4 = 53 + i2 - i3;
                int i5 = (int)(t1 * Math.pow(2, i4) + 5.6843418860808015e-14);

                throw new ArithmeticException("mplconv: Excessive FFT roundoff error --> \t" + i1 + "\t" + t1 + "\t" + i4 + "\t" + i5);
            }
        }

        int m = 0;
        int m21 = 0;
        int ms = 0;

        if (n > n1) {
            m = n - n1;
            m2 = (m << 1);
            m21 = m2 - 1;
            ms = (int)(Math.sqrt(3.0 * m21) + 5.6843418860808015e-14);
            k = n1 - m + 1;

            if (Math.abs(iq) == 1) {
                for (i = 0; i < m21; i++)
                    d1[i] = a[k + i];

                mplconv(-1, m21, ms, d1, d2, d3);
            } else {
                for (i = 0; i < m21; i++) {
                    d1[i] = a[k + i];
                    d2[i] = b[k + i];
                }

                mplconv(-2, m21, ms, d1, d2, d3);
            }

            int ii;

            for (i = 0; i < m2; i++) {
                ii = i + m2 - 2;
                c[i] -= d3[ii];
                c[i + n2] = d3[ii];
            }
        }
    }

    static void mpcbrx(PreciseNumber a, PreciseNumber b, int lnw) {
        int k = 0;
        double t1 = 0.0;
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        int na = Math.min(a.number_words, lnw);
        int ncr = (int)(Math.pow(2, pointer));

        if (na == 0) {
            zero(b);
            return;
        }
        if (a.sign == false)
            throw new ArithmeticException("mpcbrx: Argument is negative --> " + a);

        if (lnw <= ncr) {
            cbrt(a, b, lnw);
            return;
        }

        int nws = lnw;
        t1 = lnw;
        int mq = (int)(CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14); // *Cast*

        _sq(a, sk0, lnw);

        lnw = ncr + 1;

        cbrt(a, sk1, lnw);

        mpdiv(sk1, a, b, lnw);

        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;
        int iq = 0;
        int nw1 = 0;
        int nw2 = 0;

        for (k = pointer + 1; k <= mq - 1; k++) {
            nw1 = lnw;
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            nw2 = lnw;
            boolean cont = true;

            while (cont) {
                _sq(b, sk1, lnw);
                _mul(b, sk1, sk2, lnw);
                _mul(sk0, sk2, sk1, lnw);
                sub(f, sk1, sk2, lnw);

                lnw = nw1;

                _mul(b, sk2, sk1, lnw);
                mpdivd(sk1, new Chunk(3.0), sk2, lnw);

                lnw = nw2;

                add(b, sk2, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else cont = false;
            }
        }

        _mul(a, b, sk0, lnw);

        nw1 = lnw;
        lnw = Math.min((lnw << 1) - 2, nws) + 1;
        nw2 = lnw;

        _sq(sk0, sk1, lnw);
        _mul(sk0, sk1, sk2, lnw);
        sub(a, sk2, sk1, lnw);

        lnw = nw1;

        _mul(sk1, b, sk2, lnw);
        mpdivd(sk2, new Chunk(3.0), sk1, lnw);

        lnw = nw2;

        add(sk0, sk1, sk2, lnw);
        eq(sk2, b, lnw);
        round(b, nws);
    }

    static void _div(PreciseNumber a, PreciseNumber b, PreciseNumber c, int lnw) {
        int k = 0;
        double t1 = 0.0;
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        int nb = Math.min(b.number_words, lnw);
        int ncr = (int)(Math.pow(2, pointer));

        if (nb == 0)
            throw new ArithmeticException("_div: Divisor is zero");

        if (nb <= ncr) {
            mpdiv(a, b, c, lnw);
            return;
        }

        int nws = lnw;
        t1 = lnw;
        int mq = (int)(CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14);
        lnw = ncr + 1;
        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;

        mpdiv(f, b, c, lnw);

        int iq = 0;
        int nw1 = 0;
        int nw2 = 0;

        for (k = pointer + 1; k <= mq - 1; k++) {
            nw1 = lnw;
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            nw2 = lnw;
            boolean cont = true;

            while (cont) {
                _mul(b, c, sk0, lnw);
                sub(f, sk0, sk1, lnw);

                lnw = nw1;

                _mul(c, sk1, sk0, lnw);

                lnw = nw2;

                add(c, sk0, sk1, lnw);

                eq(sk1, c, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else cont = false;
            }
        }

        _mul(a, c, sk0, lnw);

        nw1 = lnw;
        lnw = Math.min((lnw << 1) - 2, nws) + 1;
        nw2 = lnw;

        _mul(sk0, b, sk1, lnw);
        sub(a, sk1, sk2, lnw);

        lnw = nw1;

        _mul(sk2, c, sk1, lnw);

        lnw = nw2;

        add(sk0, sk1, sk2, lnw);
        eq(sk2, c, lnw);
        round(c, nws);
    }

    static void _mul(PreciseNumber a, PreciseNumber b, PreciseNumber c, int lnw) {
        double t1 = 0.0;
        double t2 = 0.0;
        double t3 = 0.0;
        double t4 = 0.0;
        int i = 0;
        int na = Math.min(a.number_words, lnw);
        int nb = Math.min(b.number_words, lnw);
        int ncr = (int)(Math.pow(2, pointer));

        if (na <= ncr || nb <= ncr) {
            mul(a, b, c, lnw);
            return;
        }

        double d1[] = new double[(lnw + 2) << 1];
        double d2[] = new double[(lnw + 2) << 1];
        double d3[] = new double[(lnw + 3) << 2];
        int i2 = 0;

        for (i = 0; i < na; i++) {
            i2 = i << 1;
            t1 = a.mantissa[i];
            t2 = (int)(2.44140625e-4 * t1);
            d1[i2] = t2;
            d1[i2 + 1] = t1 - 4096.0 * t2;
        }

        for (i = na << 1; i < nb << 1; i++)
            d1[i] = 0.0;

        for (i = 0; i < nb; i++) {
            i2 = i << 1;
            t1 = b.mantissa[i];
            t2 = (int)(2.44140625e-4 * t1);
            d2[i2] = t2;
            d2[i2 + 1] = t1 - 4096.0 * t2;
        }

        for (i = (nb << 1); i < (na << 1); i++)
            d2[i] = 0.0;

        int nn = Math.max(na, nb) << 1;
        int nx = (int)(Math.sqrt(3.0 * nn) + 5.6843418860808015e-14);

        mplconv(2, nn, nx, d1, d2, d3);

        int nc = Math.min(na + nb, lnw);
        int nc1 = Math.min(lnw + 1, na + nb - 1);
        d1[0] = fSign(nc, (!(a.sign ^ b.sign)) ? 1.0 : -1.0);
        d1[1] = a.exponent + b.exponent + 1;
        d1[2] = d3[0];
        d1[nc + 2] = 0.0;
        d1[nc + 3] = 0.0;

        for (i = 1; i <= nc1; i++) {
            i2 = i << 1;
            t3 = d3[i2 - 1];
            t4 = d3[i2];
            t1 = (int)(5.9604644775390625e-8 * t3);
            t2 = t3 - 1.6777216e7 * t1;
            t3 = (int)(5.9604644775390625e-8 * t4);
            t4 -= 1.6777216e7 * t3;
            d1[i + 2] = 4096.0 * t2 + t4;
            d1[i + 1] += 4096.0 * t1 + t3;
        }
        mpnorm(d1, c, lnw);
    }

    static void _npw(PreciseNumber a, int n, PreciseNumber b, int lnw) {
        int j = 0;
        double t1 = 0.0;
        int nw2 = lnw + 2;
        PreciseNumber f1 = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw2, false);
        PreciseNumber sk1 = new PreciseNumber(nw2, false);
        int ncr = (int)(Math.pow(2, pointer));
        int na = Math.min(a.number_words, lnw);

        if (na <= ncr && n >= 0 && n <= 4) {
            mpnpwr(a, n, b, lnw);
            return;
        }

        if (na == 0) {
            if (n >= 0) {
                zero(b);
                return;
            } else throw new ArithmeticException("_npw: Argument is zero and n is negative or zero");
        }

        int nn = Math.abs(n);
        f1.sign = true;
        f1.number_words = 1;
        f1.exponent = 0;
        f1.mantissa[0] = 1;
        f1.mantissa[1] = 0;
        boolean skip = false;

        switch (nn) {
            case 0:
                eq(f1, b, lnw);
                return;

            case 1:
                eq(a, b, lnw);
                skip = true;
                break;

            case 2:
                _sq(a, b, lnw);
                skip = true;
                break;
        }

        if (!skip) {
            t1 = nn;
            int mn = (int)(CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14);

            eq(f1, b, lnw);
            eq(a, sk0, lnw);

            int kn = nn;

            for (j = 1; j <= mn; j++) {
                int kk = kn >> 1;

                if (kn != kk << 1) {
                    _mul(b, sk0, sk1, lnw);
                    eq(sk1, b, lnw);
                }

                kn = kk;

                if (j < mn) {
                    _sq(sk0, sk1, lnw);
                    eq(sk1, sk0, lnw);
                }
            }
        }

        if (n < 0) {
            _div(f1, b, sk0, lnw);
            eq(sk0, b, lnw);
        }
    }

    static void _nrt(PreciseNumber a, int n, PreciseNumber b, int lnw) {
        int k = 0;
        double t2 = 0.0;
        double tn = 0.0;
        int nw3 = lnw + 3;
        PreciseNumber f1 = new PreciseNumber(6, false);
        PreciseNumber f2 = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        PreciseNumber sk3 = new PreciseNumber(nw3, false);
        int ncr = (int)(Math.pow(2, pointer));
        int na = Math.min(a.number_words, lnw);

        if (na == 0) {
            zero(b);
            return;
        }

        if (!a.sign) throw new ArithmeticException("_nrt: Argument is negative --> " + a);
        if (n <= 0 || n > N30) throw new ArithmeticException("_nrt: Improper value of N --> " + n);

        if (lnw <= ncr) {
            mpnrt(a, n, b, lnw);
            return;
        }

        switch (n) {
            case 1:
                eq(a, b, lnw);
                return;

            case 2:
                _sqr(a, b, lnw);
                return;

            case 3:
                mpcbrx(a, b, lnw);
                return;
        }

        int nws = lnw;
        f1.number_words = 1;
        f1.sign = true;
        f1.exponent = 0;
        f1.mantissa[0] = 1;
        f1.mantissa[1] = 0;
        Chunk t1 = new Chunk();
        t1.a = lnw;
        int mq = (int)(CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14);

        sub(a, f1, sk0, lnw);

        if (sk0.number_words == 0) {
            eq(f1, b, lnw);
            return;
        }

        mdc(sk0, t1);

        int n2 = (int)(CL2 * Math.log(Math.abs(t1.a)));
        t1.a *= Math.pow(0.5, n2);
        t1.n += n2;

        if (t1.n <= -30) {
            t2 = n;
            n2 = (int)(CL2 * Math.log(t2) + 1.0 + 5.6843418860808015e-14);
            int n3 = -24 * lnw / t1.n;

            if (n3 < 1.25 * n2) {
                mpdivd(sk0, new Chunk(t2), sk1, lnw);
                add(f1, sk1, sk2, lnw);

                k = 0;

                do {
                    k++;

                    t1.a = 1 - k * n;
                    t2 = (k + 1) * n;

                    muld(sk1, new Chunk(t1.a), sk3, lnw);
                    mpdivd(sk3, new Chunk(t2), sk1, lnw);
                    _mul(sk0, sk1, sk3, lnw);
                    eq(sk3, sk1, lnw);
                    add(sk1, sk2, sk3, lnw);
                    eq(sk3, sk2, lnw);
                }
                while (sk1.number_words != 0 && sk1.exponent >= -lnw);

                eq(sk2, b, lnw);
                round(b, nws);

                return;
            }
        }

        lnw = ncr + 1;

        mpnrt(a, n, sk0, lnw);
        mpdiv(f1, sk0, b, lnw);

        tn = n;
        Chunk dpn = new Chunk(n, 0);

        dmc(dpn, f2);

        int iq = 0;
        int nw1 = 0;
        int nw2 = 0;

        for (k = pointer + 1; k <= mq; k++) {
            nw1 = lnw;
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            nw2 = lnw;
            boolean loop = true;

            while (loop) {
                _npw(b, n, sk0, lnw);
                _mul(a, sk0, sk1, lnw);
                sub(f1, sk1, sk0, lnw);

                lnw = nw1;

                _mul(b, sk0, sk1, lnw);
                mpdivd(sk1, new Chunk(tn), sk0, lnw);

                lnw = nw2;

                add(b, sk0, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else loop = false;
            }
        }

        _div(f1, b, sk0, lnw);
        eq(sk0, b, lnw);
        round(b, nws);
    }

    static void _sqr(PreciseNumber a, PreciseNumber b, int lnw) {
        int k = 0;
        int nw3 = lnw + 3;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw3, false);
        PreciseNumber sk1 = new PreciseNumber(nw3, false);
        PreciseNumber sk2 = new PreciseNumber(nw3, false);
        int na = Math.min(a.number_words, lnw);
        int ncr = (int)(Math.pow(2, pointer));

        if (na == 0) {
            zero(b);
            return;
        }

        if (!a.sign)
            throw new ArithmeticException("_sqr: Argument is negative --> " + a);

        if (lnw <= ncr) {
            mpsqrt(a, b, lnw);
            return;
        }

        int nws = lnw;
        double t1 = lnw;
        int mq = (int)(CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14);
        lnw = ncr + 1;

        mpsqrt(a, sk0, lnw);
        mpdiv(sk0, a, b, lnw);

        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        f.mantissa[0] = 1;
        f.mantissa[1] = 0;
        int iq = 0;
        int nw1 = 0;
        int nw2 = 0;

        for (k = pointer + 1; k <= mq - 1; k++) {
            nw1 = lnw;
            lnw = Math.min((lnw << 1) - 2, nws) + 1;
            nw2 = lnw;
            boolean stop = false;

            while (!stop) {
                _sq(b, sk0, lnw);
                _mul(a, sk0, sk1, lnw);
                sub(f, sk1, sk0, lnw);

                lnw = nw1;

                _mul(b, sk0, sk1, lnw);
                muld(sk1, new Chunk(0.50), sk0, lnw);

                lnw = nw2;

                add(b, sk0, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - NIT && iq == 0) iq = 1;
                else stop = true;
            }
        }

        _mul(a, b, sk0, lnw);

        nw1 = lnw;
        lnw = Math.min((lnw << 1) - 2, nws) + 1;
        nw2 = lnw;

        _sq(sk0, sk1, lnw);
        sub(a, sk1, sk2, lnw);

        lnw = nw1;

        _mul(sk2, b, sk1, lnw);
        muld(sk1, new Chunk(0.50), sk2, lnw);

        lnw = nw2;

        add(sk0, sk2, sk1, lnw);
        eq(sk1, b, lnw);
        round(b, nws);
    }

    static void _sq(PreciseNumber a, PreciseNumber b, int lnw) {
        double t1 = 0.0;
        double t2 = 0.0;
        double t3 = 0.0;
        double t4 = 0.0;
        int i = 0;
        int na = Math.min(a.number_words, lnw);
        int ncr = (int)(Math.pow(2, pointer));

        if (na == 0) {
            zero(b);
            return;
        }

        if (na <= ncr) {
            mul(a, a, b, lnw);
            return;
        }

        double d1[] = new double[(lnw + 2) << 1], d2[] = new double[(lnw + 2) << 2];
        int i2 = 0;

        for (i = 0; i < na; i++) {
            i2 = i << 1;
            t1 = a.mantissa[i];
            t2 = (int)(2.44140625e-4 * t1);
            d1[i2] = t2;
            d1[i2 + 1] = t1 - 4096.0 * t2;
        }

        int nn = (na << 1);
        int nx = (int)(Math.sqrt(3.0 * nn) + 5.6843418860808015e-14);

        mplconv(1, nn, nx, d1, d1, d2);

        int nc = Math.min((na << 1), lnw);
        int nc1 = Math.min(lnw + 1, (na << 1) - 1);
        d1[0] = nc;
        d1[1] = (a.exponent << 1) + 1;
        d1[2] = d2[0];
        d1[nc + 2] = 0.0;
        d1[nc + 3] = 0.0;

        for (i = 1; i <= nc1; i++) {
            i2 = i << 1;
            t3 = d2[i2 - 1];
            t4 = d2[i2];
            t1 = (int)(5.9604644775390625e-8 * t3);
            t2 = t3 - 1.6777216e7 * t1;
            t3 = (int)(5.9604644775390625e-8 * t4);
            t4 -= 1.6777216e7 * t3;
            d1[i + 2] = 4096.0 * t2 + t4;
            d1[i + 1] += 4096.0 * t1 + t3;
        }

        mpnorm(d1, b, lnw);
    }

    private static void mpagmx(PreciseNumber a, PreciseNumber b, int lnw) {
        int nw2 = lnw + 2;
        PreciseNumber sk0 = new PreciseNumber(nw2, false);
        PreciseNumber sk1 = new PreciseNumber(nw2, false);
        int l1 = 0;
        int s1 = 0;
        Chunk dpe1 = new Chunk(0.50, 0);

        do {
            l1++;

            if (l1 == 50)
                throw new ArithmeticException("mpagmx: Iteration limit exceeded.");

            s1 = sk0.exponent;

            add(a, b, sk0, lnw);
            muld(sk0, dpe1, sk1, lnw);
            _mul(a, b, sk0, lnw);
            _sqr(sk0, b, lnw);
            eq(sk1, a, lnw);
            sub(a, b, sk0, lnw);
        }
        while (sk0.number_words != 0.0 && (sk0.exponent < s1 || sk0.exponent >= -2));
    }

    static void _exp(PreciseNumber a, PreciseNumber pi, PreciseNumber al2, PreciseNumber b, int lnw) {
        int k = 0;
        final int nit = 1;
        int nw2 = lnw + 2;
        PreciseNumber f1 = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw2, false);
        PreciseNumber sk1 = new PreciseNumber(nw2, false);
        PreciseNumber sk2 = new PreciseNumber(nw2, false);
        int ncr = (int)(Math.pow(2, pointer));
        Chunk t1 = new Chunk();

        mdc(a, t1);

        t1.a *= Math.pow(2.0, t1.n);

        if (lnw <= ncr) {
            mpexp(a, al2, b, lnw);
            return;
        }

        Chunk t2 = new Chunk();

        mdc(al2, t2);

        if (t2.n != -24 || Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15)
            throw new ArithmeticException("_exp: LOG(2) must be precomputed.");

        mdc(pi, t2);

        if (t2.n != 0 || Math.abs(t2.a - Math.PI) > 3.552713678800501e-15)
            throw new ArithmeticException("_exp: PI must be precomputed.");

        if (t1.a >= 1e9) {
            if (t1.a > 0.0)
                throw new ArithmeticException("_exp: Argument is too large --> " + t1.a + " x 10 ^" + t1.n);
            else {
                zero(b);
                return;
            }
        }

        int nws = lnw;
        f1.sign = true;
        f1.number_words = 1;
        f1.exponent = 0;
        f1.mantissa[0] = 1;
        f1.mantissa[1] = 0;
        t2.a = nws;
        int mq = (int)(CL2 * Math.log(t2.a) + 1.0 - 5.6843418860808015e-14);

        add(a, f1, sk0, lnw);

        lnw = ncr;

        mpexp(a, al2, b, lnw);

        int iq = 0;

        for (k = pointer + 1; k <= mq; k++) {
            lnw = Math.min(lnw << 1, nws);
            boolean cont = true;

            while (cont) {
                _log(b, pi, al2, sk1, lnw);
                sub(sk0, sk1, sk2, lnw);
                _mul(b, sk2, sk1, lnw);
                eq(sk1, b, lnw);

                if (k == mq - nit && iq == 0) iq = 1;
                else cont = false;
            }
        }
    }

    static void _log(PreciseNumber a, PreciseNumber pi, PreciseNumber al2, PreciseNumber b, int lnw) {
        final int mzl = -5;
        double st = 0.0;
        double tn = 0.0;
        int nw2 = lnw + 2;
        PreciseNumber f1 = new PreciseNumber(6, false);
        PreciseNumber f4 = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw2, false);
        PreciseNumber sk1 = new PreciseNumber(nw2, false);
        PreciseNumber sk2 = new PreciseNumber(nw2, false);
        PreciseNumber sk3 = new PreciseNumber(nw2, false);
        int na = Math.min(a.number_words, lnw);
        int ncr = (int)(Math.pow(2, pointer));

        if (lnw <= ncr) {
            log(a, al2, b, lnw);
            return;
        }

        if (!a.sign || na == 0)
            throw new ArithmeticException("_log: Argument is less than or equal to zero -->" + a);

        Chunk t1 = new Chunk();

        mdc(pi, t1);

        if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15)
            throw new ArithmeticException("_log: PI must be precomputed.");

        Chunk t2 = new Chunk();
        int it2 = 0;

        if (a.number_words != 1 || a.exponent != 0 || a.mantissa[0] != 2 || !a.sign) {
            it2 = 0;

            mdc(al2, t2);

            if (t2.n != -24 || Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15)
                throw new ArithmeticException("_log: LOG(2) must be precomputed.");
        } else it2 = 1;

        f1.number_words = 1;
        f1.sign = true;
        f1.exponent = 0;
        f1.mantissa[0] = 1;
        f1.mantissa[1] = 0;
        f4.number_words = 1;
        f4.sign = true;
        f4.exponent = 0;
        f4.mantissa[0] = 4;
        f4.mantissa[1] = 0;

        sub(a, f1, sk0, lnw);

        if (sk0.number_words == 0) {
            zero(b);
            return;
        } else if (sk0.exponent <= mzl) {
            eq(sk0, sk1, lnw);
            eq(sk1, sk2, lnw);

            int i1 = 1;
            int is = 1;
            int tl = sk0.exponent - lnw - 1;

            do {
                i1++;

                is = -is;
                st = is * i1;

                _mul(sk1, sk2, sk3, lnw);
                mpdivd(sk3, new Chunk(st), sk2, lnw);
                add(sk0, sk2, sk3, lnw);
                eq(sk3, sk0, lnw);
            }
            while (sk2.exponent >= tl);

            eq(sk0, b, lnw);
            return;
        }

        mdc(a, t1);

        t2.n = 12 * lnw + 48 - t1.n;
        tn = t2.n;
        t2.a = 1.0;

        if (it2 == 1) dmc(t2, sk0);
        else muld(a, t2, sk0, lnw);

        eq(f1, sk1, lnw);
        _div(f4, sk0, sk2, lnw);
        mpagmx(sk1, sk2, lnw);
        muld(sk1, new Chunk(2.0), sk0, lnw);
        _div(pi, sk0, sk1, lnw);

        if (it2 == 1)
            mpdivd(sk1, new Chunk(tn), sk0, lnw);
        else {
            muld(al2, new Chunk(tn), sk2, lnw);
            sub(sk1, sk2, sk0, lnw);
        }
        eq(sk0, b, lnw);
    }

    static void _pi(PreciseNumber pi, int lnw) {
        int k = 0;
        double t1 = 0.0;
        int nw2 = lnw + 2;
        PreciseNumber f = new PreciseNumber(6, false);
        PreciseNumber sk0 = new PreciseNumber(nw2, false);
        PreciseNumber sk1 = new PreciseNumber(nw2, false);
        PreciseNumber sk2 = new PreciseNumber(nw2, false);
        PreciseNumber sk3 = new PreciseNumber(nw2, false);
        PreciseNumber sk4 = new PreciseNumber(nw2, false);
        int ncr = (int)(Math.pow(2, pointer));

        if (lnw <= ncr) {
            mppi(pi, lnw);
            return;
        }

        t1 = lnw * log10(1.6777216e7);
        int mq = (int)(CL2 * (Math.log(t1) - 1.0) + 1.0);
        sk0.number_words = 1;
        sk0.sign = true;
        sk0.exponent = 0;
        sk0.mantissa[0] = 1;
        f.number_words = 1;
        f.sign = true;
        f.exponent = 0;
        f.mantissa[0] = 2;
        f.mantissa[1] = 0;

        _sqr(f, sk2, lnw);
        muld(sk2, new Chunk(0.50), sk1, lnw);

        f.exponent = -1;
        f.mantissa[0] = (float)(0.50 * 1.6777216e7);

        sub(sk2, f, sk4, lnw);

        Chunk dpe1 = new Chunk(0.50);

        for (k = 1; k <= mq; k++) {
            add(sk0, sk1, sk2, lnw);
            _mul(sk0, sk1, sk3, lnw);
            _sqr(sk3, sk1, lnw);
            muld(sk2, dpe1, sk0, lnw);
            sub(sk0, sk1, sk2, lnw);
            _sq(sk2, sk3, lnw);

            t1 = Math.pow(2.0, k);

            muld(sk3, new Chunk(t1), sk2, lnw);
            sub(sk4, sk2, sk3, lnw);
            eq(sk3, sk4, lnw);
        }

        add(sk0, sk1, sk2, lnw);
        _sq(sk2, sk3, lnw);
        _div(sk3, sk4, sk2, lnw);
        eq(sk2, pi, lnw);
    }

    static int outx(PreciseNumber a, char b[], int lnw) {
        int i = 0;
        int n = 0;
        int na = Math.min(a.number_words, lnw);
        int ncr = (int)(Math.pow(2, pointer));

        if (na <= ncr) return mpoutc(a, b, lnw);

        double t1 = 0.0;
        double t2 = 0.0;
        char c1[] = new char[17];
        char c2[] = new char[17];
        char b1[] = new char[(lnw << 3) + 31], b2[] = new char[(lnw << 3) + 31];
        int nw2 = lnw + 2;
        PreciseNumber sk0 = new PreciseNumber(nw2, false);
        PreciseNumber sk1 = new PreciseNumber(nw2, false);
        PreciseNumber sk2 = new PreciseNumber(nw2, false);
        PreciseNumber sk3 = new PreciseNumber(nw2, false);
        PreciseNumber sk4 = new PreciseNumber(nw2, false);
        t1 = a.mantissa[0] + 5.9604644775390625e-8 * a.mantissa[1] + 3.552713678800501e-15 * a.mantissa[2];
        t2 = log10(t1);
        int m1 = (int)(Math.max(AL2 * 24 * (a.number_words - a.exponent) - t2, 0.0));
        Chunk dpe1 = new Chunk(10, 0);

        dmc(dpe1, sk0);
        _npw(sk0, m1, sk2, lnw);
        _mul(a, sk2, sk1, lnw);

        sk1.sign = true;
        Chunk dpe2 = new Chunk(), dpe3 = new Chunk();

        mdc(sk1, dpe2);
        Chunk.dpdec(dpe2, dpe3);

        int m2 = dpe3.n >> 1;

        _npw(sk0, m2, sk3, lnw);
        _div(sk1, sk3, sk0, lnw);
        infr(sk0, sk2, sk4, lnw);
        _mul(sk2, sk3, sk0, lnw);
        sub(sk1, sk0, sk3, lnw);

        int nws = lnw;
        lnw = sk2.number_words + 1;
        int nb1 = 0;
        int nb2 = 0;
        nb1 = outx(sk2, b1, lnw);
        lnw = sk3.number_words + 1;
        nb2 = outx(sk3, b2, lnw);
        lnw = nws;

        for (i = 0; i < 17; i++) {
            c1[i] = '\0';
            c2[i] = '\0';
        }
        for (i = 0; i < 10; i++) {
            c1[i] = b1[i + 4];
            c2[i] = b2[i + 4];
        }

        String tempStr = new String(c1);
        tempStr = tempStr.substring(0, tempStr.indexOf('\0'));
        int ie1 = Integer.parseInt(tempStr);
        tempStr = new String(c2);
        tempStr = tempStr.substring(0, tempStr.indexOf('\0'));
        int ie2 = Integer.parseInt(tempStr);
        int ie = ie1 + m2 - m1;
        c1 = (new Integer(ie)).toString().toCharArray();

        for (i = 0; i < 4; i++) b[i] = b1[i];
        for (; i < 14 - c1.length; i++) b[i] = ' ';

        int ii = 0;

        for (; i < 14; i++) b[i] = c1[ii++];
        for (i = 14; i < nb1; i++) b[i] = b1[i];

        int i2 = 0;
        int i1 = ie1 + m2 - ie2 + 19;

        if (nb1 > i1) {
            i2 = Integer.parseInt(new String(b, i1, 1));

            if (i2 >= 5) {
                boolean skip = false;

                for (i = i1 - 1; i >= 20; i--) {
                    if (b[i] != '9') {
                        skip = true;
                        break;
                    }
                    b[i] = '0';
                }
                if (!skip)
                    throw new ArithmeticException("outx: Exceptional case -- contact author.");

                i2 = Integer.parseInt(new String(b, i, 1));
                c1 = (new Integer(i2 + 1)).toString().toCharArray();
                b[i] = c1[0];
            }
        } else if (nb1 < i1)
            for (i = nb1; i < i1; i++) b[i] = '0';

        b[i1] = b2[18];
        n = Math.min(i1 + nb2 - 19, (int)(7.225 * lnw + 30));

        for (i = i1 + 1; i < n; i++) b[i] = b2[i - i1 + 19];

        if (!a.sign) b[17] = '-';

        b[n] = '\0';

        return n;
    }

}