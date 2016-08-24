package com.github.jancajthaml.number

import PreciseNumber.{inp_complex, _sq, _div, _mul, muld, dmc, eq, mul, sub, compare, ssn_complex}
import ComplexNumber._

object ComplexNumber {

  private def createComplex(size: Int): ComplexNumber = new ComplexNumber(size, false)

  def add_complex(a: ComplexNumber, 
      b: ComplexNumber, 
      c: ComplexNumber, 
      lnw: Int) {
    PreciseNumber.add(a.r, b.r, c.r, lnw)
    PreciseNumber.add(a.i, b.i, c.i, lnw)
  }

  def div_complex(a: ComplexNumber, 
      b: ComplexNumber, 
      c: ComplexNumber, 
      lnw: Int) {
    val nw2 = lnw + 2
    val f = new PreciseNumber(6, false)
    val sk0 = new PreciseNumber(nw2, false)
    val sk1 = new PreciseNumber(nw2, false)
    val sk2 = new PreciseNumber(nw2, false)
    val sk3 = new PreciseNumber(nw2, false)
    val sk4 = new PreciseNumber(nw2, false)
    val ar = a.r
    val ai = a.i
    val br = b.r
    val bi = b.i
    if (b.r.number_words == 0 && b.i.number_words == 0) throw new ArithmeticException("div_complex: Divisor is zero.")
    f.number_words = 1
    f.exponent = 0
    f.sign = true
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    mul(ar, br, sk0, lnw)
    mul(ai, bi, sk1, lnw)
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    sub(sk0, sk1, sk3, lnw)
    PreciseNumber.add(ar, ai, sk0, lnw)
    sub(br, bi, sk1, lnw)
    mul(sk0, sk1, sk4, lnw)
    sub(sk4, sk3, sk1, lnw)
    mul(br, br, sk0, lnw)
    mul(bi, bi, sk3, lnw)
    PreciseNumber.add(sk0, sk3, sk4, lnw)
    PreciseNumber.mpdiv(f, sk4, sk0, lnw)
    mul(sk2, sk0, c.r, lnw)
    mul(sk1, sk0, c.i, lnw)
  }

  def eq_complex(a: ComplexNumber, b: ComplexNumber, lnw: Int) {
    var i = 0
    val n1 = Math.min(a.r.number_words, lnw)
    val n2 = Math.min(a.i.number_words, lnw)
    b.r.sign = a.r.sign
    b.r.number_words = n1
    b.r.exponent = a.r.exponent
    b.i.sign = a.i.sign
    b.i.number_words = n2
    b.i.exponent = a.i.exponent
    i = 0
    while (i < n1) {b.r.mantissa(i) = a.r.mantissa(i)i += 1
    }
    i = 0
    while (i < n2) {b.i.mantissa(i) = a.i.mantissa(i)i += 1
    }
  }

  def mpcmul(a: ComplexNumber, 
      b: ComplexNumber, 
      c: ComplexNumber, 
      lnw: Int) {
    val nw2 = lnw + 2
    val sk0 = new PreciseNumber(nw2, false)
    val sk1 = new PreciseNumber(nw2, false)
    val sk2 = new PreciseNumber(nw2, false)
    val sk3 = new PreciseNumber(nw2, false)
    mul(a.r, b.r, sk0, lnw)
    mul(a.i, b.i, sk1, lnw)
    sub(sk0, sk1, c.r, lnw)
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    PreciseNumber.add(a.r, a.i, sk0, lnw)
    PreciseNumber.add(b.r, b.i, sk1, lnw)
    mul(sk0, sk1, sk3, lnw)
    sub(sk3, sk2, c.i, lnw)
  }

  def mpcpwr(a: ComplexNumber, 
      n: Int, 
      b: ComplexNumber, 
      lnw: Int) {
    var j: Int = 0
    var t1: Double = 0.0
    val na1 = Math.min(a.r.number_words, lnw)
    val na2 = Math.min(a.i.number_words, lnw)
    if (na1 == 0 && na2 == 0) {
      if (n >= 0) {
        zero(b)
        return
      } else throw new ArithmeticException("mpcpwr: Argument is zero and N is negative or zero.")
    }
    val nws = lnw
    lnw += 1
    val nn = Math.abs(n)
    if (nn == 0) {
      b.r.number_words = 1
      b.r.sign = true
      b.r.exponent = 0
      b.r.mantissa(0) = 1
      b.r.mantissa(1) = 0
      PreciseNumber.zero(b.i)
      return
    }
    val f = createComplex(6)
    val sk0 = createComplex(lnw + 3)
    val sk1 = createComplex(lnw + 3)
    val sk2 = createComplex(lnw + 3)
    eq_complex(a, sk0, lnw)
    f.r.number_words = 1
    f.r.sign = true
    f.r.exponent = 0
    f.r.mantissa(0) = 1
    f.r.mantissa(1) = 0
    var skip = false
    if (nn == 1) {
      eq_complex(sk0, sk2, lnw)
      skip = true
    } else if (nn == 2) {
      mpcmul(sk0, sk0, sk2, lnw)
      skip = true
    }
    if (!skip) {
      var mn: Int = 0
      var kn: Int = 0
      var kk: Int = 0
      t1 = nn
      mn = (CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14).toInt
      eq_complex(f, sk2, lnw)
      kn = nn
      j = 1
      while (j <= mn) {
        kk = kn / 2
        if (kn != 2 * kk) {
          mpcmul(sk2, sk0, sk1, lnw)
          eq_complex(sk1, sk2, lnw)
        }
        kn = kk
        if (j < mn) {
          mpcmul(sk0, sk0, sk1, lnw)
          eq_complex(sk1, sk0, lnw)
        }
        j += 1
      }
    }
    if (n < 0) {
      eq_complex(f, sk1, lnw)
      div_complex(sk1, sk2, sk0, lnw)
      eq_complex(sk0, sk2, lnw)
    }
    eq_complex(sk2, b, lnw)
    roun_complex(b, nws)
  }

  private def roun_complex(in: ComplexNumber, lnw: Int) {
    PreciseNumber.round(in.r, lnw)
    PreciseNumber.round(in.i, lnw)
  }

  def sqt_complex(a: ComplexNumber, b: ComplexNumber, lnw: Int) {
    val nw2 = lnw + 2
    val sk0 = new PreciseNumber(nw2, false)
    val sk1 = new PreciseNumber(nw2, false)
    val sk2 = new PreciseNumber(nw2, false)
    if (a.r.number_words == 0 && a.i.number_words == 0) {
      zero(b)
      return
    }
    mul(a.r, a.r, sk0, lnw)
    mul(a.i, a.i, sk1, lnw)
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    PreciseNumber.mpsqrt(sk2, sk0, lnw)
    eq(a.r, sk1, lnw)
    sk1.sign = true
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    muld(sk2, new Chunk(0.50), sk1, lnw)
    PreciseNumber.mpsqrt(sk1, sk0, lnw)
    muld(sk0, new Chunk(2.0), sk1, lnw)
    if (a.r.number_words >= 0) {
      eq(sk0, b.r, lnw)
      PreciseNumber.mpdiv(a.i, sk1, b.i, lnw)
    } else {
      PreciseNumber.mpdiv(a.i, sk1, b.r, lnw)
      b.r.sign = true
      eq(sk0, b.i, lnw)
      b.i.number_words = a.i.number_words
    }
  }

  def csub(a: ComplexNumber, 
      b: ComplexNumber, 
      c: ComplexNumber, 
      lnw: Int) {
    sub(a.r, b.r, c.r, lnw)
    sub(a.i, b.i, c.i, lnw)
  }

  private def zero(in: ComplexNumber) {
    PreciseNumber.zero(in.r)
    PreciseNumber.zero(in.i)
  }

  def _cdv(a: ComplexNumber, 
      b: ComplexNumber, 
      c: ComplexNumber, 
      lnw: Int) {
    val nw2 = lnw + 2
    val f = new PreciseNumber(6, false)
    val sk0 = new PreciseNumber(nw2, false)
    val sk1 = new PreciseNumber(nw2, false)
    val sk2 = new PreciseNumber(nw2, false)
    val sk3 = new PreciseNumber(nw2, false)
    val sk4 = new PreciseNumber(nw2, false)
    val ar = a.r
    val ai = a.i
    val br = b.r
    val bi = b.i
    if (b.r.number_words == 0 && b.i.number_words == 0) throw new ArithmeticException("_cdv: Divisor is zero.")
    f.number_words = 1
    f.exponent = 0
    f.sign = true
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    _mul(ar, br, sk0, lnw)
    _mul(ai, bi, sk1, lnw)
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    sub(sk0, sk1, sk3, lnw)
    PreciseNumber.add(ar, ai, sk0, lnw)
    sub(br, bi, sk1, lnw)
    _mul(sk0, sk1, sk4, lnw)
    sub(sk4, sk3, sk1, lnw)
    _sq(br, sk0, lnw)
    _sq(bi, sk3, lnw)
    PreciseNumber.add(sk0, sk3, sk4, lnw)
    _div(f, sk4, sk0, lnw)
    mul(sk2, sk0, c.r, lnw)
    mul(sk1, sk0, c.i, lnw)
  }

  def _cml(a: ComplexNumber, 
      b: ComplexNumber, 
      c: ComplexNumber, 
      lnw: Int) {
    val nw2 = lnw + 2
    val sk0 = new PreciseNumber(nw2, false)
    val sk1 = new PreciseNumber(nw2, false)
    val sk2 = new PreciseNumber(nw2, false)
    val sk3 = new PreciseNumber(nw2, false)
    _mul(a.r, b.r, sk0, lnw)
    _mul(a.i, b.i, sk1, lnw)
    sub(sk0, sk1, c.r, lnw)
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    PreciseNumber.add(a.r, a.i, sk0, lnw)
    PreciseNumber.add(b.r, b.i, sk1, lnw)
    _mul(sk0, sk1, sk3, lnw)
    sub(sk3, sk2, c.i, lnw)
  }

  def _cpw(a: ComplexNumber, 
      n: Int, 
      b: ComplexNumber, 
      lnw: Int) {
    var j = 0
    var t1 = 0.0
    val na1 = Math.min(a.r.number_words, lnw)
    val na2 = Math.min(a.i.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (na1 <= ncr && na2 <= ncr) {
      mpcpwr(a, n, b, lnw)
      return
    }
    if (na1 == 0 && na2 == 0) {
      if (n >= 0) {
        zero(b)
        return
      } else throw new ArithmeticException("_cpw: Argument is zero and N is negative or zero.")
    }
    val nn = Math.abs(n)
    if (nn == 0) {
      b.r.number_words = 1
      b.r.sign = true
      b.r.exponent = 0
      b.r.mantissa(0) = 1
      b.r.mantissa(1) = 0
      PreciseNumber.zero(b.i)
      return
    }
    val f = createComplex(6)
    val sk0 = createComplex(lnw + 3)
    val sk1 = createComplex(lnw + 3)
    val sk2 = createComplex(lnw + 3)
    eq_complex(a, sk0, lnw)
    f.r.number_words = 1
    f.r.sign = true
    f.r.exponent = 0
    f.r.mantissa(0) = 1
    f.r.mantissa(1) = 0
    var skip = false
    if (nn == 1) {
      eq_complex(sk0, sk2, lnw)
      skip = true
    } else if (nn == 2) {
      mpcmul(sk0, sk0, sk2, lnw)
      skip = true
    }
    if (!skip) {
      var kn: Int = 0
      var kk: Int = 0
      t1 = nn
      val mn = (CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14).toInt
      eq_complex(f, sk2, lnw)
      kn = nn
      j = 1
      while (j <= mn) {
        kk = kn / 2
        if (kn != 2 * kk) {
          _cml(sk2, sk0, sk1, lnw)
          eq_complex(sk1, sk2, lnw)
        }
        kn = kk
        if (j < mn) {
          _cml(sk0, sk0, sk1, lnw)
          eq_complex(sk1, sk0, lnw)
        }
        j += 1
      }
    }
    if (n < 0) {
      eq_complex(f, sk1, lnw)
      _cdv(sk1, sk2, sk0, lnw)
      eq_complex(sk0, sk2, lnw)
    }
    eq_complex(sk2, b, lnw)
  }

  def sq_complex(a: ComplexNumber, b: ComplexNumber, lnw: Int) {
    val nw2 = lnw + 2
    val sk0 = new PreciseNumber(nw2, false)
    val sk1 = new PreciseNumber(nw2, false)
    val sk2 = new PreciseNumber(nw2, false)
    if (a.r.number_words == 0 && a.i.number_words == 0) {
      zero(b)
      return
    }
    _sq(a.r, sk0, lnw)
    _sq(a.i, sk1, lnw)
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    _sqr(sk2, sk0, lnw)
    eq(a.r, sk1, lnw)
    sk1.sign = true
    PreciseNumber.add(sk0, sk1, sk2, lnw)
    muld(sk2, new Chunk(0.50), sk1, lnw)
    _sqr(sk1, sk0, lnw)
    muld(sk0, new Chunk(2.0), sk1, lnw)
    if (a.r.sign) {
      eq(sk0, b.r, lnw)
      _div(a.i, sk1, b.i, lnw)
    } else {
      _div(a.i, sk1, b.r, lnw)
      b.r.sign = true
      eq(sk0, b.i, lnw)
      b.i.sign = a.i.sign
    }
  }

  def _ang(x: PreciseNumber, 
      y: PreciseNumber, 
      pi: PreciseNumber, 
      a: PreciseNumber, 
      lnw: Int) {
    val ix = if (x.sign) 1 else -1
    val nx = Math.min(x.number_words, lnw)
    val iy = if (y.sign) 1 else -1
    val ny = Math.min(y.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (lnw <= ncr) {
      PreciseNumber.mpang(x, y, pi, a, lnw)
      return
    }
    if (nx == 0 && ny == 0) throw new ArithmeticException("_ang: Both arguments are zero.")
    val t1 = new Chunk()
    PreciseNumber.mdc(pi, t1)
    if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15) throw new ArithmeticException("_ang: PI must be precomputed.")
    if (nx == 0) {
      if (iy > 0) muld(pi, new Chunk(0.5), a, lnw) else muld(pi, new Chunk(-0.5), a, lnw)
      return
    } else if (ny == 0) {
      if (ix > 0) PreciseNumber.zero(a) else eq(pi, a, lnw)
      return
    }
    val sk0 = createComplex(lnw + 2)
    val sk1 = createComplex(lnw + 2)
    val sk2 = createComplex(lnw + 2)
    val sk3 = createComplex(lnw + 2)
    PreciseNumber.mdc(x, t1)
    val n2 = 24 * (lnw / 2 + 2) - t1.n
    val dpe1 = new Chunk(1.0, n2)
    muld(x, dpe1, sk0.r, lnw)
    muld(y, dpe1, sk0.i, lnw)
    sk1.r.number_words = 1
    sk1.r.sign = true
    sk1.r.exponent = 0
    sk1.r.mantissa(0) = 1
    sk1.r.mantissa(1) = 0
    PreciseNumber.zero(sk1.i)
    sk3.r.number_words = 1
    sk3.r.sign = true
    sk3.r.exponent = 0
    sk3.r.mantissa(0) = 4
    sk3.r.mantissa(1) = 0
    PreciseNumber.zero(sk3.i)
    _cdv(sk3, sk0, sk2, lnw)
    _cag(sk1, sk2, lnw)
    dpe1.a = 2.0
    dpe1.n = 0
    muld(sk1.r, dpe1, sk0.r, lnw)
    muld(sk1.i, dpe1, sk0.i, lnw)
    eq(pi, sk2.r, lnw)
    PreciseNumber.zero(sk2.i)
    _cdv(sk2, sk0, sk1, lnw)
    eq(sk1.i, a, lnw)
  }

  def _cag(a: ComplexNumber, b: ComplexNumber, lnw: Int) {
    var l1 = 0
    val sk0 = createComplex(lnw + 2)
    val sk1 = createComplex(lnw + 2)
    var s1 = 0
    val dpe1 = new Chunk(0.50, 0)
    do {
      l1 += 1
      if (l1 == 50) throw new ArithmeticException("_cag: Iteration limit exceeded.")
      s1 = sk0.r.exponent
      add_complex(a, b, sk0, lnw)
      muld(sk0.r, dpe1, sk1.r, lnw)
      muld(sk0.i, dpe1, sk1.i, lnw)
      _cml(a, b, sk0, lnw)
      sq_complex(sk0, b, lnw)
      eq_complex(sk1, a, lnw)
      sub(a.r, b.r, sk0.r, lnw)
    } while (sk0.r.number_words != 0 && (sk0.r.exponent < s1 || sk0.r.exponent >= -2));
  }

  def _cos(a: PreciseNumber, 
      pi: PreciseNumber, 
      x: PreciseNumber, 
      y: PreciseNumber, 
      lnw: Int) {
    var k = 0
    var t2 = 0.0
    val nit = 1
    val f1 = new PreciseNumber(6, false)
    val na = Math.min(a.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (lnw <= ncr) {
      ssn_complex(a, pi, x, y, lnw)
      return
    }
    if (na == 0) {
      x.sign = true
      x.number_words = 1
      x.exponent = 0
      x.mantissa(0) = 1
      PreciseNumber.zero(y)
      return
    }
    val t1 = new Chunk()
    PreciseNumber.mdc(pi, t1)
    if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15) throw new ArithmeticException("cssx: PI must be precomputed.")
    f1.number_words = 1
    f1.sign = true
    f1.exponent = 0
    f1.mantissa(0) = 1
    f1.mantissa(1) = 0
    val nws = lnw
    val sk0 = createComplex(lnw + 2)
    val sk1 = createComplex(lnw + 2)
    val sk2 = createComplex(lnw + 2)
    val sk3 = createComplex(lnw + 2)
    muld(pi, new Chunk(2.0), sk0.r, lnw)
    _div(a, sk0.r, sk1.r, lnw)
    PreciseNumber.nint(sk1.r, sk2.r, lnw)
    mul(sk2.r, sk0.r, sk1.r, lnw)
    sub(a, sk1.r, sk0.r, lnw)
    t2 = nws
    val mq = (CL2 * Math.log(t2) + 1.0 - 5.6843418860808015e-14).toInt
    eq(f1, sk2.r, lnw)
    lnw = ncr
    ssn_complex(sk0.r, pi, sk3.r, sk3.i, lnw)
    var iq = 0
    k = pointer + 1
    while (k <= mq) {
      lnw = Math.min(lnw << 1, nws)
      var cont = true
      while (cont) {
        _ang(sk3.r, sk3.i, pi, sk1.r, lnw)
        sub(sk0.r, sk1.r, sk2.i, lnw)
        _cml(sk3, sk2, sk1, lnw)
        eq_complex(sk1, sk3, lnw)
        if (k == mq - nit && iq == 0) iq = 1 else cont = false
      }
      k += 1
    }
    _sq(sk3.r, sk0.r, lnw)
    _sq(sk3.i, sk0.i, lnw)
    PreciseNumber.add(sk0.r, sk0.i, sk1.r, lnw)
    _sqr(sk1.r, sk2.r, lnw)
    _div(sk3.r, sk2.r, sk0.r, lnw)
    _div(sk3.i, sk2.r, sk0.i, lnw)
    eq(sk0.r, x, lnw)
    eq(sk0.i, y, lnw)
  }
}

class ComplexNumber private (size: Int, b: Boolean) extends Shared {

  var r: PreciseNumber = new PreciseNumber(size, b)

  var i: PreciseNumber = new PreciseNumber(size, b)

  private def constHelper(precision: Int): Int = {
    val maxnw = precisionToSize(precision)
    r = new PreciseNumber(maxnw, false)
    i = new PreciseNumber(maxnw, false)
    maxnw
  }

  def this() {
    this()
    constHelper(precision_digits)
  }

  def this(b: Boolean, precision: Int) {
    this()
    constHelper(precision)
  }

  def this(real: Double, precision: Int) {
    this()
    constHelper(precision)
    dmc(new Chunk(real), r)
  }

  def this(dc: Complex) {
    this(dc, precision_digits)
  }

  def this(dc: Complex, precision: Int) {
    this()
    constHelper(precision)
    dmc(new Chunk(dc.real()), r)
    dmc(new Chunk(dc.aimag().toDouble), i)
  }

  def this(real: String) {
    this(real, precision_digits)
  }

  def this(real: String, precision: Int) {
    this()
    inp_complex(real.toCharArray(), real.length, r, Math.min(nw, constHelper(precision) - 2))
  }

  def this(real: PreciseNumber) {
    this()
    val maxnw = real.maxnw
    r = new PreciseNumber(maxnw, false)
    i = new PreciseNumber(maxnw, false)
    eq(real, r, nw)
  }

  def this(in: ComplexNumber) {
    this()
    r = new PreciseNumber(in.r.maxnw, false)
    i = new PreciseNumber(in.i.maxnw, false)
    eq_complex(in, this, nw)
  }

  def this(real: Int, imag: Int) {
    this(real.toDouble, imag.toDouble, precision_digits)
  }

  def this(real: Int, imag: Int, precision: Int) {
    this(real.toDouble, imag.toDouble, precision)
  }

  def this(real: Double, imag: Double) {
    this(real, imag, precision_digits)
  }

  def this(real: Double, imag: Double, precision: Int) {
    this()
    constHelper(precision)
    dmc(new Chunk(real), r)
    dmc(new Chunk(imag), i)
  }

  def this(real: String, imag: String) {
    this(real, imag, precision_digits)
  }

  def this(real: String, imag: String, precision: Int) {
    this()
    val lnw = Math.min(nw, constHelper(precision) - 1)
    inp_complex(real.toCharArray(), real.length, r, lnw)
    inp_complex(imag.toCharArray(), imag.length, i, lnw)
  }

  def this(real: PreciseNumber, imag: PreciseNumber) {
    this()
    r = new PreciseNumber(real.maxnw, false)
    i = new PreciseNumber(imag.maxnw, false)
    eq(real, r, nw)
    eq(imag, i, nw)
  }

  def assign(zb: ComplexNumber): ComplexNumber = {
    eq_complex(zb, this, Math.min(nw, r.maxnw))
    this
  }

  def add(ja: ComplexNumber): ComplexNumber = {
    val res = new ComplexNumber()
    add_complex(this, ja, res, nw)
    res
  }

  def subtract(ja: ComplexNumber): ComplexNumber = {
    val res = new ComplexNumber()
    csub(this, ja, res, nw)
    res
  }

  def negate(): ComplexNumber = {
    val res = new ComplexNumber()
    eq_complex(this, res, nw)
    res.r.sign = !res.r.sign
    res.i.sign = !res.i.sign
    res
  }

  def multiply(ja: ComplexNumber): ComplexNumber = {
    val res = new ComplexNumber()
    _cml(this, ja, res, nw)
    res
  }

  def divide(ja: ComplexNumber): ComplexNumber = {
    val res = new ComplexNumber()
    _cdv(this, ja, res, nw)
    res
  }

  override def equals(o: Any): Boolean = {
    if (o == this) return true
    try {
      0 == compare(r, o.asInstanceOf[ComplexNumber].r, nw) && 
        0 == compare(i, o.asInstanceOf[ComplexNumber].i, nw)
    } catch {
      case cce: ClassCastException => false
    }
  }

  override def toString(): String = (r.toString + i.toString)

  def abs(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    val mpt2 = new PreciseNumber()
    val mpt3 = new PreciseNumber()
    _mul(r, r, mpt1, nw)
    _mul(i, i, mpt2, nw)
    PreciseNumber.add(mpt1, mpt2, mpt3, nw)
    _sqr(mpt3, res, nw)
    res
  }

  def real(): RealNumber = {
    val res = new RealNumber()
    eq(r, res, nw)
    res
  }

  def aimag(): RealNumber = {
    val res = new RealNumber()
    eq(i, res, nw)
    res
  }

  def conjg(): ComplexNumber = {
    val res = new ComplexNumber()
    eq_complex(this, res, nw)
    res.i.sign = !i.sign
    res
  }

  def doubleValue(): Double = r.doubleValue()

  def intValue(): Int = r.intValue()

  def floatValue(): Float = r.floatValue()

  def complexValue(): Complex = {
    new Complex(r.toDPE().value(), i.toDPE().value())
  }

  def sqrt(): ComplexNumber = {
    val res = new ComplexNumber()
    sqt_complex(this, res, nw)
    res
  }

  def pow(exponent: Int): ComplexNumber = {
    val res = new ComplexNumber()
    _cpw(this, exponent, res, nw)
    res
  }

}
