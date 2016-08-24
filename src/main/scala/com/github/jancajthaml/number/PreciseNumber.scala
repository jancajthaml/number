package com.github.jancajthaml.number

import PrecisionNumber._

private[number] object PrecisionNumber {

  var t30: Double = 0

  var r30: Double = 0

  var sd: Double = 314159265

  def getPrecisionInDigits(): Int = precision_digits

  def add(a: PrecisionNumber, 
      b: PrecisionNumber, 
      c: PrecisionNumber, 
      lnw: Int) {
    var i = 0
    val na = Math.min(a.number_words, lnw)
    val nb = Math.min(b.number_words, lnw)
    val d = Array.ofDim[Double](lnw + 5)
    val db = if ((a.sign == b.sign)) 1.0 else -1.0
    val ixa = a.exponent
    val ixb = b.exponent
    val ish = ixa - ixb
    var nd = 0
    var ixd = 0
    if (ish >= 0) {
      val m1 = Math.min(na, ish)
      val m2 = Math.min(na, nb + ish)
      val m3 = na
      val m4 = Math.min(Math.max(na, ish), lnw + 1)
      val m5 = Math.min(Math.max(na, nb + ish), lnw + 1)
      d(0) = 0
      d(1) = 0
      i = 0
      while (i < m1) {d(i + 2) = a.mantissa(i)i += 1
      }
      i = m1
      while (i < m2) {d(i + 2) = a.mantissa(i) + db * b.mantissa(i - ish)i += 1
      }
      i = m2
      while (i < m3) {d(i + 2) = a.mantissa(i)i += 1
      }
      i = m3
      while (i < m4) {d(i + 2) = 0.0i += 1
      }
      i = m4
      while (i < m5) {d(i + 2) = db * b.mantissa(i - ish)i += 1
      }
      nd = m5
      ixd = ixa
      d(nd + 2) = 0.0
      d(nd + 3) = 0.0
    } else {
      val nsh = -ish
      val m1 = Math.min(nb, nsh)
      val m2 = Math.min(nb, na + nsh)
      val m3 = nb
      val m4 = Math.min(Math.max(nb, nsh), lnw + 1)
      val m5 = Math.min(Math.max(nb, na + nsh), lnw + 1)
      d(0) = 0
      d(1) = 0
      i = 0
      while (i < m1) {
        d(i + 2) = db * b.mantissa(i)i += 1
      }
      i = m1
      while (i < m2) {
        d(i + 2) = a.mantissa(i - nsh) + db * b.mantissa(i)i += 1
      }
      i = m2
      while (i < m3) {
        d(i + 2) = db * b.mantissa(i)i += 1
      }
      i = m3
      while (i < m4) {
        d(i + 2) = 0.0i += 1
      }
      i = m4
      while (i < m5) {
        d(i + 2) = a.mantissa(i - nsh)i += 1
      }
      nd = m5
      ixd = ixb
      d(nd + 2) = 0.0
      d(nd + 3) = 0.0
    }
    d(0) = fSign(nd, if (a.sign) 1 else -1)
    d(1) = ixd
    mpnorm(d, c, lnw)
  }

  def cbrt(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    val lnw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(lnw3, false)
    val sk1 = new PrecisionNumber(lnw3, false)
    val sk2 = new PrecisionNumber(lnw3, false)
    val na = Math.min(a.number_words, lnw)
    if (na == 0) {
      zero(b)
      return
    }
    if (a.sign == false) throw new ArithmeticException("mpcbrt: argument is negative --> " + a)
    val nws = lnw
    val t1 = new Chunk()
    t1.a = lnw
    val mq = (CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14).toInt
    lnw += 1
    mul(a, a, sk0, lnw)
    mdc(a, t1)
    val t2 = new Chunk()
    t2.n = -(t1.n << 1) / 3
    t2.a = Math.pow((t1.a * Math.pow(2.0, (t1.n + 3.0 * t2.n / 2.0))), (-2.0 / 3.0))
    dmc(t2, b)
    f.sign = true
    f.number_words = 1
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    lnw = 3
    var iq = 0
    var k = 2
    while (k <= mq - 1) {
      val nw1 = lnw
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      val nw2 = lnw
      var cont = true
      while (cont) {
        mul(b, b, sk1, lnw)
        mul(b, sk1, sk2, lnw)
        mul(sk0, sk2, sk1, lnw)
        sub(f, sk1, sk2, lnw)
        lnw = nw1
        mul(b, sk2, sk1, lnw)
        mpdivd(sk1, new Chunk(3.0), sk2, lnw)
        lnw = nw2
        add(b, sk2, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else cont = false
      }
      k += 1
    }
    mul(a, b, sk0, lnw)
    val nw1 = lnw
    lnw = Math.min((lnw << 1) - 2, nws) + 1
    val nw2 = lnw
    mul(sk0, sk0, sk1, lnw)
    mul(sk0, sk1, sk2, lnw)
    sub(a, sk2, sk1, lnw)
    lnw = nw1
    mul(sk1, b, sk2, lnw)
    mpdivd(sk2, new Chunk(3.0), sk1, lnw)
    lnw = nw2
    add(sk0, sk1, sk2, lnw)
    eq(sk2, b, lnw)
    round(b, nws)
  }

  def compare(a: PrecisionNumber, b: PrecisionNumber, lnw: Int): Int = {
    var i = 0
    var ic = 0
    var ia = (if (a.sign) 1 else -1)
    if (a.number_words == 0.) ia = 0
    var ib = (if (b.sign) 1 else -1)
    if (b.number_words == 0.) ib = 0
    val na = Math.min(a.number_words, lnw)
    val nb = Math.min(b.number_words, lnw)
    if (ia != ib) ic = (fSign(1, ia - ib)).toInt else if (a.exponent != b.exponent) ic = (ia * fSign(1, 
      a.exponent - b.exponent)).toInt else {
      var sameMantissas = true
      i = 0
      while (i < Math.min(na, nb)) {
        if (a.mantissa(i) != b.mantissa(i)) {
          ic = (ia * fSign(1., a.mantissa(i) - b.mantissa(i))).toInt
          sameMantissas = false
          //break
        }
        i += 1
      }
      if (sameMantissas) ic = if ((na != nb)) (ia * fSign(1, na - nb)).toInt else 0
    }
    ic
  }

  def mpdiv(a: PrecisionNumber, 
      b: PrecisionNumber, 
      c: PrecisionNumber, 
      lnw: Int) {
    var i = 0
    var j = 0
    var rb = 0.0
    var ss = 0.0
    var t0 = 0.0
    var t1 = 0.0
    var t2 = 0.0
    val na = Math.min(a.number_words, lnw)
    val nb = Math.min(b.number_words, lnw)
    if (na == 0) {
      zero(c)
      return
    }
    if (nb == 1 && b.mantissa(0) == 1.) {
      c.number_words = na
      c.sign = !(a.sign ^ b.sign)
      c.exponent = a.exponent - b.exponent
      i = 0
      while (i < na) {c.mantissa(i) = a.mantissa(i)i += 1
      }
      return
    }
    if (nb == 0) throw new ArithmeticException("mpdiv: Divisor is zero.")
    val d = Array.ofDim[Double](lnw + 4)
    t0 = 1.6777216e7 * b.mantissa(0)
    if (nb >= 2) t0 += b.mantissa(1)
    if (nb >= 3) t0 += 5.9604644775390625e-8 * b.mantissa(2)
    if (nb >= 4) t0 += 3.552713678800501e-15 * b.mantissa(3)
    rb = 1.0 / t0
    val md = Math.min(na + nb, lnw)
    d(0) = 0.0
    i = 1
    while (i <= na) {d(i) = a.mantissa(i - 1)i += 1
    }
    i = na + 1
    while (i < md + 4) {d(i) = 0.0i += 1
    }
    j = 2
    while (j <= na + 1) {
      t1 = 2.81474976710656e14 * d(j - 2) + 1.6777216e7 * d(j - 1) + 
        d(j) + 
        5.9604644775390625e-8 * d(j + 1)
      t0 = (rb * t1).toInt
      val j3 = j - 3
      val i2 = Math.min(nb, lnw + 2 - j3) + 2
      val ij = i2 + j3
      i = 2
      while (i < i2) {d(i + j3) -= t0 * b.mantissa(i - 2)i += 1
      }
      if (((j - 1) % 32) == 0) {
        i = j
        while (i < ij) {
          t1 = d(i)
          t2 = (5.9604644775390625e-8 * t1).toInt
          d(i) = t1 - 1.6777216e7 * t2
          d(i - 1) += t2
          i += 1
        }
      }
      d(j - 1) += 1.6777216e7 * d(j - 2)
      d(j - 2) = t0
      j += 1
    }
    var stopped = false
    j = na + 2
    while (j <= lnw + 3) {
      t1 = 2.81474976710656e14 * d(j - 2) + 1.6777216e7 * d(j - 1) + 
        d(j)
      if (j <= lnw + 2) t1 = t1 + 5.9604644775390625e-8 * d(j + 1)
      t0 = (rb * t1).toInt
      val j3 = j - 3
      val i2 = Math.min(nb, lnw + 2 - j3) + 2
      val ij = i2 + j3
      ss = 0.0
      var i3 = 0
      i = 2
      while (i < i2) {
        i3 = i + j3
        d(i3) -= t0 * b.mantissa(i - 2)
        ss = ss + Math.abs(d(i3))
        i += 1
      }
      if (((j - 1) % 32) == 0) {
        i = j
        while (i < ij) {
          t1 = d(i)
          t2 = (5.9604644775390625e-8 * t1).toInt
          d(i) = t1 - 1.6777216e7 * t2
          d(i - 1) += t2
          i += 1
        }
      }
      d(j - 1) += 1.6777216e7 * d(j - 2)
      d(j - 2) = t0
      if (ss == 0.0) {
        stopped = true
        //break
      }
      if (ij <= lnw + 1) d(ij + 2) = 0.0
      j += 1
    }
    if (!stopped) j = lnw + 3
    d(j - 1) = 0.0
    val is = if ((d(0) == 0.0)) 1 else 2
    val nc = Math.min(j - 1, lnw)
    d(nc + 2) = 0.0
    d(nc + 3) = 0.0
    i = j
    while (i >= 2) {d(i) = d(i - is)i -= 1
    }
    d(0) = fSign(nc, if ((!(a.sign ^ b.sign))) 1 else -1)
    d(1) = a.exponent - b.exponent + is - 2
    mpnorm(d, c, lnw)
  }

  def mpdivd(a: PrecisionNumber, 
      b: Chunk, 
      c: PrecisionNumber, 
      lnw: Int) {
    var j = 0
    var k = 0
    var bb = 0.0
    var br = 0.0
    var dd = 0.0
    var t1 = 0.0
    val f = new PrecisionNumber(6, false)
    val na = Math.min(a.number_words, lnw)
    val ib = (fSign(1.0, b.a)).toInt
    if (na == 0) {
      zero(c)
      return
    }
    if (b.a == 0.0) throw new ArithmeticException("mpdivd: Divisor is zero")
    var n1 = b.n / 24
    val n2 = b.n - 24 * n1
    bb = Math.abs(b.a) * Math.pow(2.0, n2)
    if (bb >= 1.6777216e7) {
      k = 1
      while (k <= 100) {
        bb = 5.9604644775390625e-8 * bb
        if (bb < 1.6777216e7) {
          n1 += k
          //break
        }
        k += 1
      }
    } else if (bb < 1.0) {
      k = 1
      while (k <= 100) {
        bb = 1.6777216e7 * bb
        if (bb >= 1.0) {
          n1 -= k
          //break
        }
        k += 1
      }
    }
    if (bb != (bb).toInt) {
      dmc(new Chunk(fSign(bb, b.a), n1 * 24), f)
      mpdiv(a, f, c, lnw)
      return
    }
    val d = Array.ofDim[Double](lnw + 4)
    br = 1.0 / bb
    dd = a.mantissa(0)
    var skipJ = false
    j = 2
    while (j <= lnw + 3) {
      t1 = (br * dd).toInt
      d(j) = t1
      dd = 1.6777216e7 * (dd - t1 * bb)
      if (j <= na) dd = dd + a.mantissa(j - 1) else if (dd == 0.0) {
        skipJ = true
        //break
      }
      j += 1
    }
    if (!skipJ) j = lnw + 3
    val nc = Math.min(j - 1, lnw)
    d(0) = fSign(nc, (if (a.sign) 1 else -1) * ib)
    d(1) = a.exponent - n1
    if (j <= lnw + 2) d(j + 1) = 0.0
    if (j <= lnw + 1) d(j + 2) = 0.0
    mpnorm(d, c, lnw)
  }

  def dmc(a: Chunk, b: PrecisionNumber) {
    var i = 0
    var k = 0
    var aa = 0.0
    if (a.a == 0.0) {
      zero(b)
      return
    }
    var n1 = a.n / 24
    val n2 = a.n - 24 * n1
    aa = Math.abs(a.a) * Math.pow(2.0, n2)
    if (aa >= 1.6777216e7) {
      k = 1
      while (k <= 100) {
        aa = 5.9604644775390625e-8 * aa
        if (aa < 1.6777216e7) {
          n1 = n1 + k
          //break
        }
        k += 1
      }
    } else if (aa < 1.0) {
      k = 1
      while (k <= 100) {
        aa = 1.6777216e7 * aa
        if (aa >= 1.0) {
          n1 = n1 - k
          //break
        }
        k += 1
      }
    }
    b.exponent = n1
    b.mantissa(0) = (aa).toInt.toFloat
    aa = 1.6777216e7 * (aa - b.mantissa(0))
    b.mantissa(1) = (aa).toInt.toFloat
    aa = 1.6777216e7 * (aa - b.mantissa(1))
    b.mantissa(2) = (aa).toInt.toFloat
    aa = 1.6777216e7 * (aa - b.mantissa(2))
    b.mantissa(3) = (aa).toInt.toFloat
    b.mantissa(4) = 0
    b.mantissa(5) = 0
    i = 3
    while (i >= 0) {if (b.mantissa(i) != 0.) //breaki -= 1
    }
    aa = i + 1
    b.sign = (a.a >= 0)
    b.number_words = (aa).toInt
  }

  def eq(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    val na = Math.min(a.number_words, lnw)
    if (na == 0) {
      zero(b)
      return
    }
    b.number_words = na
    b.sign = a.sign
    b.exponent = a.exponent
    var i = 0
    while (i <= na) {b.mantissa(i) = a.mantissa(i)i += 1
    }
  }

  def infr(a: PrecisionNumber, 
      b: PrecisionNumber, 
      c: PrecisionNumber, 
      lnw: Int) {
    val na = Math.min(a.number_words, lnw)
    val ma = a.exponent
    if (na == 0) {
      zero(b)
      zero(c)
    }
    if (ma >= lnw - 1) throw new ArithmeticException("infr: Argument is too large -->" + a)
    var i = 0
    val nb = Math.min(Math.max(ma + 1, 0), na)
    if (nb == 0) zero(b) else {
      b.number_words = nb
      b.sign = a.sign
      b.exponent = ma
      b.mantissa(nb) = 0
      b.mantissa(nb + 1) = 0
      i = 0
      while (i < nb) {
        b.mantissa(i) = a.mantissa(i)i += 1
      }
    }
    val nc = na - nb
    if (nc <= 0) zero(c) else {
      c.number_words = nc
      c.sign = a.sign
      c.exponent = ma - nb
      c.mantissa(nc) = 0
      c.mantissa(nc + 1) = 0
      i = 0
      while (i < nc) {
        c.mantissa(i) = a.mantissa(i + nb)i += 1
      }
    }
    round(b, lnw)
    round(c, lnw)
  }

  def mdc(a: PrecisionNumber, b: Chunk) {
    var aa = 0.0
    var isAZero = false
    if (a.number_words == 0) {
      b.a = 0.0
      b.n = 0
      isAZero = true
    }
    if (!isAZero) {
      val na = a.number_words
      aa = a.mantissa(0)
      if (na >= 2) aa += 5.9604644775390625e-8 * a.mantissa(1)
      if (na >= 3) aa += 3.552713678800501e-15 * a.mantissa(2)
      if (na >= 4) aa += 5.9604644775390625e-8 * 3.552713678800501e-15 * a.mantissa(3)
      b.n = 24 * a.exponent
      b.a = fSign(aa, if (a.sign) 1.0 else -1.0)
    }
  }

  def mul(a: PrecisionNumber, 
      b: PrecisionNumber, 
      c: PrecisionNumber, 
      lnw: Int) {
    var i = 0
    var j = 0
    var t1 = 0.0
    var t2 = 0.0
    val na = Math.min(a.number_words, lnw)
    val nb = Math.min(b.number_words, lnw)
    if (na == 0 || nb == 0) {
      zero(c)
      return
    }
    if (na == 1 && a.mantissa(0) == 1) {
      c.sign = !(a.sign ^ b.sign)
      c.number_words = nb
      c.exponent = a.exponent + b.exponent
      i = 0
      while (i < nb) {
        c.mantissa(i) = b.mantissa(i)i += 1
      }
      return
    } else if (nb == 1 && b.mantissa(0) == 1.) {
      c.sign = !(a.sign ^ b.sign)
      c.number_words = na
      c.exponent = a.exponent + b.exponent
      i = 0
      while (i < na) {
        c.mantissa(i) = a.mantissa(i)i += 1
      }
      return
    }
    val d = Array.ofDim[Double](lnw + 4)
    val nc = Math.min(na + nb, lnw)
    var d2 = a.exponent + b.exponent
    i = 0
    while (i < nc + 4) {d(i) = 0.0i += 1
    }
    j = 3
    while (j <= na + 2) {
      t1 = a.mantissa(j - 3)
      val j3 = j - 3
      val n2 = Math.min(nb + 2, lnw + 4 - j3)
      i = 2
      while (i < n2) {
        d(i + j3) += t1 * b.mantissa(i - 2)i += 1
      }
      if (((j - 2) % 32) == 0) {
        val i1 = Math.max(3, j - 32)
        val i2 = n2 + j3
        i = i1 - 1
        while (i < i2) {
          t1 = d(i)
          t2 = (5.9604644775390625e-8 * t1).toInt
          d(i) = t1 - 1.6777216e7 * t2
          d(i - 1) += t2
          i += 1
        }
      }
      j += 1
    }
    if (d(1) != 0.0) {
      d2 += 1.0
      i = nc + 3
      while (i >= 2) {
        d(i) = d(i - 1)i -= 1
      }
    }
    d(0) = fSign(nc, if ((!(a.sign ^ b.sign))) 1.0 else -1.0)
    d(1) = d2
    mpnorm(d, c, lnw)
  }

  def muld(a: PrecisionNumber, 
      b: Chunk, 
      c: PrecisionNumber, 
      lnw: Int) {
    var i = 0
    var k = 0
    var bb = 0.0
    val f = new PrecisionNumber(6, false)
    val na = Math.min(a.number_words, lnw)
    val ib = (fSign(1.0, b.a)).toInt
    if (na == 0 || b.a == 0.0) {
      zero(c)
      return
    }
    var n1 = b.n / 24
    val n2 = b.n - 24 * n1
    bb = Math.abs(b.a) * Math.pow(2.0, n2)
    if (bb >= 1.6777216e7) {
      k = 1
      while (k <= 100) {
        bb = 5.9604644775390625e-8 * bb
        if (bb < 1.6777216e7) {
          n1 += k
          //break
        }
        k += 1
      }
    } else if (bb < 1.0) {
      k = 1
      while (k <= 100) {
        bb = 1.6777216e7 * bb
        if (bb >= 1.0) {
          n1 -= k
          //break
        }
        k += 1
      }
    }
    if (bb != (bb).toInt) {
      dmc(new Chunk(fSign(bb, b.a), n1 * 24), f)
      mul(f, a, c, lnw)
      return
    }
    val d = Array.ofDim[Double](lnw + 4)
    i = 2
    while (i < na + 2) {
      d(i) = bb * a.mantissa(i - 2)i += 1
    }
    d(0) = fSign(na, (if (a.sign) 1 else -1) * ib)
    d(1) = a.exponent + n1
    d(na + 2) = 0.0
    d(na + 3) = 0.0
    mpnorm(d, c, lnw)
  }

  def nint(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    var i = 0
    val f = new PrecisionNumber(6, false)
    val s = new PrecisionNumber(lnw + 2, false)
    val na = Math.min(a.number_words, lnw)
    if (na == 0) {
      zero(b)
      return
    }
    if (a.exponent >= lnw) throw new ArithmeticException("nint: Argument is too large --> " + a)
    f.number_words = 1
    f.sign = true
    f.exponent = -1
    f.mantissa(0) = (0.5 * 1.6777216e7).toFloat
    f.mantissa(1) = 0
    if (a.sign) add(a, f, s, lnw) else sub(a, f, s, lnw)
    val ic = if (s.sign) 1 else -1
    val nc = s.number_words
    val mc = s.exponent
    val nb = Math.min(Math.max(mc + 1, 0), nc)
    if (nb == 0) zero(b) else {
      b.number_words = nb
      b.sign = (ic >= 0)
      b.exponent = mc
      b.mantissa(nb) = 0
      b.mantissa(nb + 1) = 0
      i = 0
      while (i < nb) {b.mantissa(i) = s.mantissa(i)i += 1
      }
    }
  }

  private def mpnorm(d: Array[Double], a: PrecisionNumber, lnw: Int) {
    val risc = true
    var t1 = 0.0
    var t2 = 0.0
    var t3 = 0.0
    var i = 0
    var ia = (fSign(1.0, d(0))).toInt
    var na = Math.min((Math.abs(d(0))).toInt, lnw)
    if (na == 0) {
      zero(a)
      return
    }
    val n4 = na + 4
    var a2 = d(1)
    d(1) = 0.0
    var needToNormalize = true
    while (needToNormalize) {
      var breakLoop = false
      if (!risc) {
        var s1 = 0.0
        var k = 0
        if (na > 8) {
          k = 1
          while (k <= 3) {
            s1 = 0.0
            i = 2
            while (i < n4) {
              t2 = 5.9604644775390625e-8 * d(i)
              t1 = (t2).toInt
              if (t2 < 0.0 && t1 != t2) t1 -= 1
              d(i) -= t1 * 1.6777216e7
              d(i - 1) += t1
              s1 += Math.abs(t1)
              i += 1
            }
            if (s1 == 0.0) {
              breakLoop = true
              //break
            }
            k += 1
          }
        }
      }
      if (!breakLoop) {
        t1 = 0
        i = n4 - 1
        while (i >= 2) {
          t3 = t1 + d(i)
          t2 = 5.9604644775390625e-8 * (t3)
          t1 = (t2).toInt
          if (t2 < 0.0 && t1 != t2) t1 -= 1
          d(i) = t3 - t1 * 1.6777216e7
          i -= 1
        }
        d(1) += t1
      }
      if (d(1) < 0.0) {
        ia = -ia
        d(2) += 1.6777216e7 * d(1)
        d(1) = 0.0
        i = 1
        while (i < n4) {d(i) = -d(i)i += 1
        }
      } else if (d(1) > 0.0) {
        i = n4 - 2
        while (i >= 1) {a.mantissa(i - 1) = d(i).toFloati -= 1
        }
        na = Math.min(na + 1, lnw)
        a2 += 1
        needToNormalize = false
      } else {
        i = 2
        while (i < n4) {a.mantissa(i - 2) = d(i).toFloati += 1
        }
        needToNormalize = false
      }
    }
    a.number_words = na
    a.sign = (ia >= 0)
    a.exponent = (a2).toInt
    round(a, lnw)
  }

  def mpnpwr(a: PrecisionNumber, 
      n: Int, 
      b: PrecisionNumber, 
      lnw: Int) {
    var j = 0
    var t1 = 0.0
    val lnw3 = lnw + 3
    val f1 = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(lnw3, false)
    val sk1 = new PrecisionNumber(lnw3, false)
    val na = Math.min(a.number_words, lnw)
    if (na == 0) {
      if (n >= 0) {
        zero(b)
        return
      } else throw new ArithmeticException("mpnpwr: Argument is zero and n is negative or zero --> " + 
        a + 
        "\n" + 
        n)
    }
    val nws = lnw
    lnw += 1
    val nn = Math.abs(n)
    f1.sign = true
    f1.number_words = 1
    f1.exponent = 0
    f1.mantissa(0) = 1
    f1.mantissa(1) = 0
    var skip = false
    nn match {
      case 0 => 
        eq(f1, b, lnw)
        return

      case 1 => 
        eq(a, b, lnw)
        skip = true

      case 2 => 
        mul(a, a, sk0, lnw)
        eq(sk0, b, lnw)
        skip = true

    }
    if (!skip) {
      t1 = nn
      val mn = (CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14).toInt
      eq(f1, b, lnw)
      eq(a, sk0, lnw)
      var kn = nn
      j = 1
      while (j <= mn) {
        val kk = kn >> 1
        if (kn != kk << 1) {
          mul(b, sk0, sk1, lnw)
          eq(sk1, b, lnw)
        }
        kn = kk
        if (j < mn) {
          mul(sk0, sk0, sk1, lnw)
          eq(sk1, sk0, lnw)
        }
        j += 1
      }
    }
    if (n < 0) {
      mpdiv(f1, b, sk0, lnw)
      eq(sk0, b, lnw)
    }
    round(b, nws)
  }

  def mpnrt(a: PrecisionNumber, 
      n: Int, 
      b: PrecisionNumber, 
      lnw: Int) {
    var k = 0
    var t2 = 0.0
    var tn = 0.0
    val lnw3 = lnw + 3
    val f1 = new PrecisionNumber(6, false)
    val f2 = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(lnw3, false)
    val sk1 = new PrecisionNumber(lnw3, false)
    val sk2 = new PrecisionNumber(lnw3, false)
    val sk3 = new PrecisionNumber(lnw3, false)
    val na = Math.min(a.number_words, lnw)
    if (na == 0) {
      zero(b)
      return
    }
    if (!a.sign) throw new ArithmeticException("mpnrt: Argument is negative -->" + a)
    if (n <= 0 || n > N30) throw new ArithmeticException("mpnrt: Improper value of n -->" + n)
    n match {
      case 1 => 
        eq(a, b, lnw)
        return

      case 2 => 
        mpsqrt(a, b, lnw)
        return

      case 3 => 
        cbrt(a, b, lnw)
        return

    }
    val nws = lnw
    f1.number_words = 1
    f1.sign = true
    f1.exponent = 0
    f1.mantissa(0) = 1
    f1.mantissa(1) = 0
    val t1 = new Chunk()
    t1.a = lnw
    val mq = (CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14).toInt
    sub(a, f1, sk0, lnw)
    if (sk0.number_words == 0) {
      eq(f1, b, lnw)
      return
    }
    mdc(sk0, t1)
    var n2 = (CL2 * Math.log(Math.abs(t1.a))).toInt
    t1.a *= Math.pow(0.5, n2)
    t1.n += n2
    if (t1.n <= -30) {
      t2 = n
      n2 = (CL2 * Math.log(t2) + 1.0 + 5.6843418860808015e-14).toInt
      val n3 = -24 * lnw / t1.n
      if (n3 < 1.25 * n2) {
        lnw += 1
        mpdivd(sk0, new Chunk(t2), sk1, lnw)
        add(f1, sk1, sk2, lnw)
        k = 0
        val temp = t1.n
        t1.n = 0
        do {
          k += 1
          t1.a = 1 - k * n
          t2 = (k + 1) * n
          muld(sk1, t1, sk3, lnw)
          mpdivd(sk3, new Chunk(t2), sk1, lnw)
          mul(sk0, sk1, sk3, lnw)
          eq(sk3, sk1, lnw)
          add(sk1, sk2, sk3, lnw)
          eq(sk3, sk2, lnw)
        } while (sk1.number_words != 0 && sk1.exponent >= -lnw);
        t1.n = temp
        eq(sk2, b, lnw)
        mpdiv(f1, sk2, sk0, lnw)
        round(b, nws)
        return
      }
    }
    tn = n
    val dpn = new Chunk(n, 0)
    mdc(a, t1)
    val dp1 = new Chunk()
    dp1.n = (-t1.n / tn).toInt
    dp1.a = Math.exp(-1.0 / tn * (Math.log(t1.a) + (t1.n + tn * dp1.n) * ALT))
    dmc(dp1, b)
    dmc(dpn, f2)
    lnw = 3
    var iq = 0
    k = 2
    while (k <= mq) {
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      var loop = true
      while (loop) {
        mpnpwr(b, n, sk0, lnw)
        mul(a, sk0, sk1, lnw)
        sub(f1, sk1, sk0, lnw)
        mul(b, sk0, sk1, lnw)
        mpdivd(sk1, new Chunk(tn), sk0, lnw)
        add(b, sk0, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else loop = false
      }
      k += 1
    }
    mpdiv(f1, b, sk1, lnw)
    eq(sk1, b, lnw)
    round(b, nws)
  }

  def round(a: PrecisionNumber, lnw: Int) {
    var i = 0
    var a2 = a.exponent
    a.exponent = 0
    var na = Math.min(a.number_words, lnw)
    val n1 = na + 1
    if (a.mantissa(0) == 0) {
      var allZero = true
      i = 1
      while (i <= n1) {
        if (a.mantissa(i) != 0) {
          allZero = false
          //break
        }
        i += 1
      }
      if (allZero) {
        zero(a)
        return
      }
      val k = i
      i = 0
      while (i <= n1 - k) {a.mantissa(i) = a.mantissa(i + k)i += 1
      }
      a2 -= k
      na -= Math.max(k - 2, 0)
    }
    if (na == lnw && round >= 1) {
      if ((round == 1) && (a.mantissa(na) >= 0.5 * 1.6777216e7) || 
        (round == 2) && (a.mantissa(na) >= 1)) a.mantissa(na - 1) += 1
      var loopBreak = false
      i = na - 1
      while (i >= 0) {
        if (a.mantissa(i) < 1.6777216e7) {
          loopBreak = true
          //break
        }
        a.mantissa(i) -= 1.6777216e7.toFloat
        if (i != 0) a.mantissa(i - 1) += 1 else a.exponent += 1
        i -= 1
      }
      if (!loopBreak) {
        a.mantissa(0) = a.exponent.toFloat
        na = 1
        a2 += 1
      }
    }
    try {
      if (a.mantissa(na - 1) == 0) {
        var allZero = true
        i = na - 1
        while (i >= 0) {
          if (a.mantissa(i) != 0) {
            allZero = false
            //break
          }
          i -= 1
        }
        if (allZero) {
          zero(a)
          return
        }
        na = i + 1
      }
    } catch {
      case e: ArrayIndexOutOfBoundsException => 
    }
    if (a2 < -2.e6) throw new ArithmeticException("round: Exponent underflow.") else if (a2 > 2.e6) throw new ArithmeticException("round: Exponent overflow.")
    if (a.mantissa(0) == 0) zero(a) else {
      a.number_words = na
      a.exponent = a2
      a.mantissa(na) = 0
      a.mantissa(na + 1) = 0
    }
  }

  def mpsqrt(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    var k = 0
    var t2 = 0.0
    val lnw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(lnw3, false)
    val sk1 = new PrecisionNumber(lnw3, false)
    val sk2 = new PrecisionNumber(lnw3, false)
    val na = Math.min(a.number_words, lnw)
    if (na == 0) {
      zero(b)
      return
    }
    if (!a.sign) throw new ArithmeticException("mpsqrt: Argument is negative --> " + a)
    val nws = lnw
    val t1 = new Chunk(lnw, 0)
    val mq = (CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14).toInt
    var iq = 0
    mdc(a, t1)
    val dp1 = new Chunk()
    dp1.n = -t1.n >> 1
    t2 = Math.sqrt(t1.a * Math.pow(2.0, (t1.n + (dp1.n << 1))))
    dp1.a = 1.0 / t2
    dmc(dp1, b)
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    lnw = 3
    iq = 0
    var nw1 = 0
    var nw2 = 0
    k = 2
    while (k <= mq - 1) {
      nw1 = lnw
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      nw2 = lnw
      var stop = false
      while (!stop) {
        mul(b, b, sk0, lnw)
        mul(a, sk0, sk1, lnw)
        sub(f, sk1, sk0, lnw)
        lnw = nw1
        mul(b, sk0, sk1, lnw)
        muld(sk1, new Chunk(0.50), sk0, lnw)
        lnw = nw2
        add(b, sk0, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else stop = true
      }
      k += 1
    }
    mul(a, b, sk0, lnw)
    nw1 = lnw
    lnw = Math.min((lnw << 1) - 2, nws) + 1
    nw2 = lnw
    mul(sk0, sk0, sk1, lnw)
    sub(a, sk1, sk2, lnw)
    lnw = nw1
    mul(sk2, b, sk1, lnw)
    muld(sk1, new Chunk(0.50), sk2, lnw)
    lnw = nw2
    add(sk0, sk2, sk1, lnw)
    eq(sk1, b, lnw)
    round(b, nws)
  }

  def sub(a: PrecisionNumber, 
      b: PrecisionNumber, 
      c: PrecisionNumber, 
      lnw: Int) {
    val bb = new PrecisionNumber(0, false)
    bb.sign = !b.sign
    bb.number_words = b.number_words
    bb.exponent = b.exponent
    bb.mantissa = b.mantissa
    add(a, bb, c, lnw)
    bb.mantissa = null
  }

  def zero(in: PrecisionNumber) {
    in.number_words = 0
    in.sign = true
    in.exponent = 0
  }

  def inp_complex(a: Array[Char], 
      n: Int, 
      b: PrecisionNumber, 
      lnw: Int) {
    var i = 0
    var j = 0
    var is = 0
    var id = 0
    var bi = 0.0
    var ai = 0
    val ca = Array.ofDim[Char](81)
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val nws = lnw += 1
    var i1 = 0
    var nn = 0
    var caretFound = false
    i = 0
    while (i < n) {
      ai = a(i)
      if (ai == '^') {
        caretFound = true
        //break
      }
      if (ai == '.' || ai == '+' || ai == '-') //break
      i += 1
    }
    j = 0
    while (j < 81) {ca(j) = '\0'j += 1
    }
    if (caretFound) {
      var i2 = i - 1
      if (i2 > 79) throw new NumberFormatException("inp_complex: Syntax error in literal string.")
      j = 0
      i = 0
      while (i <= i2) {
        ai = a(i)
        if (ai == ' ') //continue else if (!java.lang.Character.isDigit(ai)) throw new NumberFormatException("inp_complex: Syntax error in literal string.")
        ca(j += 1) = ai
        i += 1
      }
      if (ca(0) != '1' || ca(1) != '0') throw new NumberFormatException("inp_complex: Syntax error in literal string.")
      i1 = i2 + 2
      var exit = true
      i = i1
      while (i < n) {
        ai = a(i)
        if (ai == 'x' || ai == '*') {
          exit = false
          //break
        }
        i += 1
      }
      if (exit) throw new NumberFormatException("inp_complex: Syntax error in literal string.")
      i2 = i - 1
      val l1 = i2 - i1
      if (l1 > 79) throw new NumberFormatException("inp_complex: Syntax error in literal string.")
      id = 0
      is = 1
      j = 0
      i = 0
      while (i <= l1) {
        ai = a(i + i1)
        if (ai == ' ' || ai == '+') //continue else if (ai == '-' && id == 0) {
          id = 1
          is = -1
        } else {
          if (!java.lang.Character.isDigit(ai)) throw new NumberFormatException("inp_complex: Syntax error in literal string.")
          id = 1
          ca(j += 1) = ai
        }
        i += 1
      }
      ca(j) = '\0'
      nn = java.lang.Integer.parseInt(new String(ca, 0, j))
      nn = is * nn
      i1 = i2 + 2
    }
    var exit = true
    i = i1
    while (i < n) {
      if (a(i) != ' ') {
        exit = false
        //break
      }
      i += 1
    }
    if (exit) throw new NumberFormatException("inp_complex: Syntax error in literal string.")
    i1 = i
    if (a(i1) == '+') {
      i1 += 1
      is = 1
    } else if (a(i1) == '-') {
      i1 = i1 + 1
      is = -1
    } else is = 1
    var ib = 0
    id = 0
    var ip = 0
    zero(sk2)
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    var it = 0
    var mm = 0
    var cont = true
    while (cont) {
      ip = 0
      mm = 0
      while (mm < 6) {ca(mm) = '0'mm += 1
      }
      i = i1
      while (i < n) {
        ai = a(i)
        if (ai == ' ')  else if (ai == '.') {
          if (ip != 0) throw new NumberFormatException("inp_complex: Syntax error in literal string.")
          ip = id
        } else if (!java.lang.Character.isDigit(ai)) throw new NumberFormatException("inp_complex: Syntax error in literal string.") else {
          id += 1
          ca(ib += 1) = ai
        }
        if (ib == 6 || i == (n - 1) && ib != 0) {
          if (it != 0) {
            ca(ib) = '\0'
            bi = java.lang.Integer.parseInt(new String(ca, 0, ib))
            muld(sk2, new Chunk(1e6), sk0, lnw)
            if (bi != 0) {
              f.number_words = 1
              f.sign = true
              f.mantissa(0) = bi.toFloat
            } else {
              f.number_words = 0
              f.sign = true
            }
            add(sk0, f, sk2, lnw)
            mm = 0
            while (mm < 6) {ca(mm) = '0'mm += 1
            }
          }
          if ((i + 1) != n) ib = 0
        }
        i += 1
      }
      if (it == 0) {
        ib = 6 - ib
        if (ib == 6) ib = 0
        it = 1
      } else cont = false
    }
    if (is == -1) sk2.sign = !sk2.sign
    if (ip == 0) ip = id
    nn += ip - id
    f.number_words = 1
    f.sign = true
    f.mantissa(0) = 10
    mpnpwr(f, nn, sk0, lnw)
    mul(sk2, sk0, sk1, lnw)
    eq(sk1, b, lnw)
    round(b, nws)
  }

  def mpoutc(a: PrecisionNumber, b: Array[Char], lnw: Int): Int = {
    var i = 0
    var j = 0
    var k = 0
    var nn = 0
    var n = 0
    var aa = 0.0
    var t1 = 0.0
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val na = Math.min(a.number_words, lnw)
    lnw += 1
    f.sign = true
    f.number_words = 1
    f.exponent = 0
    f.mantissa(0) = 10
    var nx = 0
    if (na != 0) {
      aa = a.mantissa(0)
      if (na >= 2) aa += 5.9604644775390625e-8 * a.mantissa(1)
      if (na >= 3) aa += 3.552713678800501e-15 * a.mantissa(2)
      if (na >= 4) aa += 5.9604644775390625e-8 * 3.552713678800501e-15 * a.mantissa(3)
      t1 = AL2 * 24 * a.exponent + log10(aa)
      nx = if ((t1 >= 0.0)) t1.toInt else (t1 - 1.0).toInt
      mpnpwr(f, nx, sk0, lnw)
      mpdiv(a, sk0, sk1, lnw)
      var cont = true
      while (cont) {
        if (sk1.exponent < 0) {
          nx = nx - 1
          muld(sk1, new Chunk(10.0), sk0, lnw)
          eq(sk0, sk1, lnw)
        } else if (sk1.mantissa(0) >= 10) {
          nx += 1
          mpdivd(sk1, new Chunk(10.0), sk0, lnw)
          eq(sk0, sk1, lnw)
        } else cont = false
      }
      sk1.sign = true
    } else nx = 0
    b(0) = '1'
    b(1) = '0'
    b(2) = ' '
    b(3) = '^'
    var ca = String.valueOf(nx).toCharArray()
    val len = ca.length
    val blank = 14 - len
    i = 4
    while (i < blank) {b(i) = ' 'i += 1
    }
    i = 0
    while (i < len) {b(blank + i) = ca(i)i += 1
    }
    b(14) = ' '
    b(15) = 'x'
    b(16) = ' '
    b(17) = if ((a.sign == false)) '-' else ' '
    nn = if ((na != 0)) sk1.mantissa(0).toInt else 0
    ca = String.valueOf(nn).toCharArray()
    b(18) = ca(0)
    b(19) = '.'
    var ix = 20
    if (na == 0) {
      b(ix) = '\0'
      return ix
    }
    f.mantissa(0) = nn.toFloat
    sub(sk1, f, sk0, lnw)
    if (sk0.number_words == 0) {
      b(ix) = '\0'
      return ix
    }
    muld(sk0, new Chunk(1e6), sk1, lnw)
    val nl = (Math.max(lnw * log10(1.6777216e7) / 6.0 - 1.0, 1.0)).toInt
    var skip = false
    j = 1
    while (j <= nl) {
      if (sk1.exponent == 0.) {
        nn = (sk1.mantissa(0)).toInt
        f.number_words = 1
        f.sign = true
        f.mantissa(0) = nn.toFloat
      } else {
        f.number_words = 0
        f.sign = true
        nn = 0
      }
      ca = String.valueOf(nn).toCharArray()
      i = 0
      while (i < 6 - ca.length) {b(i + ix) = '0'i += 1
      }
      k = 0
      while (i < 6) {b(i + ix) = ca(k += 1)i += 1
      }
      ix += 6
      sub(sk1, f, sk0, lnw)
      muld(sk0, new Chunk(1e6), sk1, lnw)
      if (sk1.number_words == 0) {
        skip = true
        //break
      }
      j += 1
    }
    if (!skip) j = nl + 1
    val l = ix
    if (b(l) == '0' || 
      (j > nl && b(l - 1) == '0' && b(l - 2) == '0' && b(l - 3) == '0')) {
      b(l) = '\0'
      var loopbreak = false
      i = l - 1
      while (i >= 20) {
        if (b(i) != '0') {
          ix = i
          loopbreak = true
          //break
        }
        b(i) = '\0'
        i -= 1
      }
      if (!loopbreak) ix = 20
    } else if (j > nl && b(l - 1) == '9' && b(l - 2) == '9' && b(l - 3) == '9') {
      b(l) = '\0'
      skip = false
      i = l - 1
      while (i >= 20) {
        if (b(i) != '9') {
          skip = true
          //break
        }
        b(i) = '\0'
        i -= 1
      }
      if (!skip) {
        ix = 20
        if (b(18) == '9') {
          b(18) = '1'
          ca = String.valueOf(nx + 1).toCharArray()
          k = 0
          i = 0
          while (i < 10 - ca.length) {b(i + 4) = ' 'i += 1
          }
          while (i < 10) {b(i + 4) = ca(k += 1)i += 1
          }
        } else {
          ca(0) = b(18)
          ca(1) = '\0'
          nn = java.lang.Integer.parseInt(new String(ca, 0, 1))
          ca = String.valueOf(nn + 1).toCharArray()
          b(18) = ca(0)
        }
      } else {
        ca(0) = b(i)
        ca(1) = '\0'
        nn = java.lang.Integer.parseInt(new String(ca, 0, 1))
        ca = String.valueOf(nn + 1).toCharArray()
        b(i) = ca(0)
        ix = i
      }
    }
    n = ix
    b(n) = '\0'
    n
  }

  def dexc(a: Array[Char], 
      l: Int, 
      b: PrecisionNumber, 
      lnw: Int) {
    var i = 0
    var foundExponent = false
    i = 0
    while (i < l) {
      if (a(i) == 'D' || a(i) == 'E' || a(i) == 'd' || a(i) == 'e') {
        foundExponent = true
        //break
      }
      i += 1
    }
    if (!foundExponent) {
      inp_complex(a, l, b, lnw)
      return
    }
    val c = Array.ofDim[Char](precision_digits + 101)
    val i1 = i + 1
    val l1 = i
    val l2 = l - i1
    c(0) = '1'
    c(1) = '0'
    c(2) = '^'
    i = 0
    while (i < l2) {c(i + 3) = a(i + i1)i += 1
    }
    c(l2 + 3) = 'x'
    i = 0
    while (i < l1) {c(i + l2 + 4) = a(i)i += 1
    }
    c(i + l2 + 4) = '\0'
    inp_complex(c, l1 + l2 + 4, b, lnw)
  }

  def mpouts(a: PrecisionNumber, 
      la: Int, 
      cs: Array[Char], 
      lnw: Int): Int = {
    lnw = Math.min(lnw, (la / log10(1.6777216e7) + 2.0).toInt)
    Math.min(if ((precision_digits < 10000)) mpoutc(a, cs, lnw) else outx(a, cs, lnw), la + 20) + 
      1
  }
}

class PrecisionNumber(in: PrecisionNumber) extends Shared with Cloneable {

  var maxnw: Int = in.maxnw

  var sign: Boolean = false

  var number_words: Int = 0

  var exponent: Int = 0

  var mantissa: Float = null

  if (maxnw > 0) {
    mantissa = Array.ofDim[Float](maxnw)
    eq(in, this, nw)
  } else {
    exponent = in.exponent
    sign = in.sign
    number_words = in.number_words
    mantissa = null
  }

  def this() {
    this(mp2, false)
  }

  def this(maxNW: Int, b: Boolean) {
    this()
    maxnw = maxNW
    sign = true
    number_words = 0
    exponent = 0
    if (maxnw >= 1) {
      mantissa = Array.ofDim[Float](maxnw)
      mantissa(0) = 0
    } else mantissa = null
  }

  def this(b: Boolean, precision: Int) {
    this()
    maxnw = (precisionToSize(precision))
    mantissa = Array.ofDim[Float](maxnw)
    mantissa(0) = 0
    sign = true
    number_words = 0
    exponent = 0
  }

  def this(ia: Double, precision: Int) {
    this()
    maxnw = precisionToSize(precision)
    mantissa = Array.ofDim[Float](maxnw)
    dmc(new Chunk(ia), this)
  }

  def this(str: String) {
    this(str, precision_digits)
  }

  def this(str: String, precision: Int) {
    this()
    val temp = str.toCharArray()
    maxnw = precisionToSize(precision)
    mantissa = Array.ofDim[Float](maxnw)
    dexc(temp, temp.length, this, Math.min(nw, maxnw - 2))
  }

  def clone(): AnyRef = new PrecisionNumber(this)

  override def toString(): String = {
    val res = new StringBuffer()
    val az = Array.ofDim[Char](precision_digits + 100)
    val nd = mpouts(this, precision_digits_current, az, nw)
    var i = 0
    i = 0
    while (i < nd) {res.append(az(i))i += 1
    }
    val old = res.toString
    val xx = old.indexOf("x")
    val exponent = old.substring(old.indexOf("^") + 1, xx).trim()
    val ex = java.lang.Integer.parseInt(exponent)
    old.substring(xx + 1).trim() + 
      (if (ex != 0) ("e" + (if ((ex < 0)) "" else "+") + exponent) else "")
  }

  def getExponent(): Int = {
    val res = new StringBuffer()
    val az = Array.ofDim[Char](precision_digits + 100)
    val nd = mpouts(this, precision_digits_current, az, nw)
    var i = 0
    i = 0
    while (i < nd) {res.append(az(i))i += 1
    }
    val old = res.toString
    val hat = old.indexOf("^")
    val xx = old.indexOf("x")
    val exponent = old.substring(hat + 1, xx).trim()
    java.lang.Integer.parseInt(exponent)
  }

  override def equals(o: Any): Boolean = {
    if (o == this) return true
    try {
      0 == compare(this, o.asInstanceOf[PrecisionNumber], nw)
    } catch {
      case cce: ClassCastException => false
    }
  }

  def compareTo(o: AnyRef): Int = {
    compare(this, o.asInstanceOf[PrecisionNumber], nw)
  }

  def isNegative(): Boolean = {
    (compareTo(new PrecisionNumber(true, 10)) < 0)
  }

  def doubleValue(): Double = this.toDPE().value()

  def floatValue(): Float = this.toDPE().value().toFloat

  def intValue(): Int = this.toDPE().value().toInt

  def longValue(): Long = this.toDPE().value().toLong

  def shortValue(): Short = this.toDPE().value().toShort

  def toDPE(): Chunk = {
    val res = new Chunk()
    mdc(this, res)
    res
  }

  
  def mpang(x: PrecisionNumber, 
      y: PrecisionNumber, 
      pi: PrecisionNumber, 
      a: PrecisionNumber, 
      lnw: Int) {
    var k = 0
    val nw3 = lnw + 3
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val sk3 = new PrecisionNumber(nw3, false)
    val sk4 = new PrecisionNumber(nw3, false)
    val ix = if (x.sign) 1 else -1
    val nx = Math.min(x.number_words, lnw)
    val iy = if (y.sign) 1 else -1
    val ny = Math.min(y.number_words, lnw)
    if (nx == 0 && ny == 0) throw new ArithmeticException("mpang: Both arguments are zero.")
    val t1 = new Chunk()
    mdc(pi, t1)
    if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15) throw new ArithmeticException("mpang: PI must be precomputed")
    if (nx == 0) {
      muld(pi, new Chunk(if ((iy > 0)) 0.5 else -0.5), a, lnw)
      return
    } else if (ny == 0) {
      if (ix > 0) zero(a) else eq(pi, a, lnw)
      return
    }
    val nws = lnw
    lnw += 1
    t1.a = nws
    val mq = (CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14).toInt
    mul(x, x, sk0, lnw)
    mul(y, y, sk1, lnw)
    add(sk0, sk1, sk2, lnw)
    mpsqrt(sk2, sk3, lnw)
    mpdiv(x, sk3, sk1, lnw)
    mpdiv(y, sk3, sk2, lnw)
    mdc(sk1, t1)
    val t2 = new Chunk()
    mdc(sk2, t2)
    t1.n = Math.max(t1.n, -66)
    t2.n = Math.max(t2.n, -66)
    t1.a = t1.a * Math.pow(2.0, t1.n)
    t2.a = t2.a * Math.pow(2.0, t2.n)
    val t3 = new Chunk()
    t3.a = Math.atan2(t2.a, t1.a)
    dmc(t3, a)
    var kk: Int = 0
    if (Math.abs(t1.a) <= Math.abs(t2.a)) {
      kk = 1
      eq(sk1, sk0, lnw)
    } else {
      kk = 2
      eq(sk2, sk0, lnw)
    }
    lnw = 3
    var iq = 0
    k = 2
    while (k <= mq) {
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      var cont = true
      while (cont) {
        ssn_complex(a, pi, sk1, sk2, lnw)
        if (kk == 1) {
          sub(sk0, sk1, sk3, lnw)
          mpdiv(sk3, sk2, sk4, lnw)
          sub(a, sk4, sk1, lnw)
        } else {
          sub(sk0, sk2, sk3, lnw)
          mpdiv(sk3, sk1, sk4, lnw)
          add(a, sk4, sk1, lnw)
        }
        eq(sk1, a, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else cont = false
      }
      k += 1
    }
    round(a, nws)
  }

  def mpcssh(a: PrecisionNumber, 
      al2: PrecisionNumber, 
      x: PrecisionNumber, 
      y: PrecisionNumber, 
      lnw: Int) {
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val sk3 = new PrecisionNumber(nw3, false)
    val nws = lnw
    lnw += 1
    f.sign = true
    f.number_words = 1
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    mpexp(a, al2, sk0, lnw)
    mpdiv(f, sk0, sk1, lnw)
    add(sk0, sk1, sk2, lnw)
    muld(sk2, new Chunk(0.5), sk3, lnw)
    eq(sk3, x, lnw)
    sub(sk0, sk1, sk2, lnw)
    muld(sk2, new Chunk(0.5), sk3, lnw)
    eq(sk3, y, lnw)
    round(x, nws)
    round(y, nws)
  }

  def ssn_complex(a: PrecisionNumber, 
      pi: PrecisionNumber, 
      x: PrecisionNumber, 
      y: PrecisionNumber, 
      lnw: Int) {
    var t2 = 0.0
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val sk3 = new PrecisionNumber(nw3, false)
    val sk4 = new PrecisionNumber(nw3, false)
    val sk5 = new PrecisionNumber(nw3, false)
    val sk6 = new PrecisionNumber(nw3, false)
    val na = Math.min(a.number_words, lnw)
    var l1 = 0
    if (na == 0) {
      x.sign = true
      x.number_words = 1
      x.exponent = 0
      x.mantissa(0) = 1
      zero(y)
      l1 = 0
      return
    }
    val t1 = new Chunk()
    mdc(pi, t1)
    if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15) throw new ArithmeticException("mpccsn: pi must be precomputed.")
    val nws = lnw
    lnw += 1
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    muld(pi, new Chunk(2.0), sk0, lnw)
    mpdiv(a, sk0, sk1, lnw)
    nint(sk1, sk2, lnw)
    sub(sk1, sk2, sk3, lnw)
    mdc(sk3, t1)
    var ka = 0
    var kb = 0
    if (t1.n >= -24) {
      t1.a *= Math.pow(2.0, t1.n)
      t2 = 4.0 * t1.a
      ka = (nint(t2)).toInt
      kb = (nint(8.0 * (t2 - ka))).toInt
    } else {
      ka = 0
      kb = 0
    }
    t1.a = ((ka << 3) + kb) / 32.0
    t1.n = 0
    dmc(t1, sk1)
    sub(sk3, sk1, sk2, lnw)
    mul(sk0, sk2, sk1, lnw)
    if (sk1.number_words == 0) {
      zero(sk0)
      l1 = 0
    } else {
      eq(sk1, sk0, lnw)
      mul(sk0, sk0, sk2, lnw)
      l1 = 0
      do {
        l1 = l1 + 1
        if (l1 == 10000) throw new ArithmeticException("ssn_complex: Iteration limit exceeded.")
        t2 = -(2.0 * l1) * (2.0 * l1 + 1.0)
        mul(sk2, sk1, sk3, lnw)
        mpdivd(sk3, new Chunk(t2), sk1, lnw)
        add(sk1, sk0, sk3, lnw)
        eq(sk3, sk0, lnw)
      } while (sk1.number_words != 0 && sk1.exponent >= sk0.exponent - lnw);
    }
    eq(sk0, sk1, lnw)
    mul(sk0, sk0, sk2, lnw)
    sub(f, sk2, sk3, lnw)
    mpsqrt(sk3, sk0, lnw)
    var kc = 0
    kc = Math.abs(kb)
    f.mantissa(0) = 2
    if (kc == 0) {
      sk2.sign = true
      sk2.number_words = 1
      sk2.exponent = 0
      sk2.mantissa(0) = 1
      zero(sk3)
    } else kc match {
      case 1 => 
        mpsqrt(f, sk4, lnw)
        add(f, sk4, sk5, lnw)
        mpsqrt(sk5, sk4, lnw)

      case 2 => mpsqrt(f, sk4, lnw)
      case 3 => 
        mpsqrt(f, sk4, lnw)
        sub(f, sk4, sk5, lnw)
        mpsqrt(sk5, sk4, lnw)

      case 4 => zero(sk4)
    }
    if (kb < 0) sk3.sign = !sk3.sign
    mul(sk0, sk2, sk4, lnw)
    mul(sk1, sk3, sk5, lnw)
    sub(sk4, sk5, sk6, lnw)
    mul(sk1, sk2, sk4, lnw)
    mul(sk0, sk3, sk5, lnw)
    add(sk4, sk5, sk1, lnw)
    eq(sk6, sk0, lnw)
    ka match {
      case 0 => 
        eq(sk0, x, lnw)
        eq(sk1, y, lnw)

      case 1 => 
        eq(sk1, x, lnw)
        x.sign = !x.sign
        eq(sk0, y, lnw)

      case -1 => 
        eq(sk1, x, lnw)
        eq(sk0, y, lnw)
        y.sign = !y.sign

      case 2 | -2 => 
        eq(sk0, x, lnw)
        x.sign = !x.sign
        eq(sk1, y, lnw)
        y.sign = !y.sign

    }
    round(x, nws)
    round(y, nws)
  }

  def mpexp(a: PrecisionNumber, 
      al2: PrecisionNumber, 
      b: PrecisionNumber, 
      lnw: Int) {
    var i = 0
    var l1 = 0
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val sk3 = new PrecisionNumber(nw3, false)
    val t1 = new Chunk()
    mdc(a, t1)
    t1.a = t1.value()
    val t2 = new Chunk()
    if (Math.abs(t1.a - ALT) > 5.9604644775390625e-8) {
      mdc(al2, t2)
      if (t2.n != -24 || 
        Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15) throw new ArithmeticException("mpexp: LOG (2) must be precomputed.")
    }
    if (t1.a >= 1e9) {
      if (t1.a > 0.0) throw new ArithmeticException("MPEXP: Argument is too large --> " + t1.a + " x 10 ^" + 
        t1.n) else {
        zero(b)
        l1 = 0
        return
      }
    }
    val nws = lnw
    lnw += 1
    f.sign = true
    f.number_words = 1
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    var nz = 0
    if (Math.abs(t1.a - ALT) > 5.9604644775390625e-8) {
      mpdiv(a, al2, sk0, lnw)
      nint(sk0, sk1, lnw)
      mdc(sk1, t1)
      nz = (t1.value() + fSign(5.6843418860808015e-14, t1.a)).toInt
      mul(al2, sk1, sk2, lnw)
      sub(a, sk2, sk0, lnw)
    } else {
      eq(a, sk0, lnw)
      nz = 0
    }
    val tl = sk0.exponent - lnw
    var skip = false
    if (sk0.number_words == 0) {
      sk0.number_words = 1
      sk0.sign = true
      sk0.exponent = 0
      l1 = 0
      skip = true
    }
    if (!skip) {
      mpdivd(sk0, new Chunk(1.0, 8), sk1, lnw)
      eq(f, sk2, lnw)
      eq(f, sk3, lnw)
      l1 = 0
      t2.n = 0
      do {
        l1 = l1 + 1
        if (l1 == 10000) throw new ArithmeticException("mpexp: Iteration limit exceeded.")
        t2.a = l1
        mul(sk2, sk1, sk0, lnw)
        mpdivd(sk0, t2, sk2, lnw)
        add(sk3, sk2, sk0, lnw)
        eq(sk0, sk3, lnw)
      } while (sk2.number_words != 0. && sk2.exponent >= tl);
      i = 1
      while (i <= 8) {
        mul(sk0, sk0, sk1, lnw)
        eq(sk1, sk0, lnw)
        i += 1
      }
    }
    muld(sk0, new Chunk(1.0, nz), sk1, lnw)
    eq(sk1, b, lnw)
    round(b, nws)
  }

  def log(a: PrecisionNumber, 
      al2: PrecisionNumber, 
      b: PrecisionNumber, 
      lnw: Int) {
    var k = 0
    val nw3 = lnw + 3
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val na = Math.min(a.number_words, lnw)
    if (a.sign == false || na == 0) throw new ArithmeticException("log: Argument is less than or equal to zero -->" + a)
    val t1 = new Chunk()
    val t2 = new Chunk()
    mdc(a, t1)
    if (Math.abs(t1.a - 2.0) > 1e-3 || t1.n != 0) {
      mdc(al2, t2)
      if (t2.n != -24 || 
        Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15) throw new ArithmeticException("log: LOG (2) must be precomputed.")
    }
    if (a.number_words == 1 && a.sign && a.exponent == 0 && a.mantissa(0) == 1.) {
      b.number_words = 0
      b.exponent = 0
      b.sign = true
      return
    }
    val nws = lnw
    t2.a = nws
    val mq = (CL2 * Math.log(t2.a) + 1.0 - 5.6843418860808015e-14).toInt
    t1.a = Math.log(t1.a) + t1.n * ALT
    t1.n = 0
    dmc(t1, b)
    lnw = 3
    var iq = 0
    k = 2
    while (k <= mq) {
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      var cont = true
      while (cont) {
        mpexp(b, al2, sk0, lnw)
        sub(a, sk0, sk1, lnw)
        mpdiv(sk1, sk0, sk2, lnw)
        add(b, sk2, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else cont = false
      }
      k += 1
    }
    round(b, nws)
  }

  def mppi(pi: PrecisionNumber, lnw: Int) {
    var k = 0
    var t1 = 0.0
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val sk3 = new PrecisionNumber(nw3, false)
    val sk4 = new PrecisionNumber(nw3, false)
    val nws = lnw
    lnw += 1
    t1 = nws * log10(1.6777216e7)
    val mq = (CL2 * (Math.log(t1) - 1.0) + 1.0).toInt
    sk0.number_words = 1
    sk0.sign = true
    sk0.exponent = 0
    sk0.mantissa(0) = 1
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    f.mantissa(0) = 2
    f.mantissa(1) = 0
    mpsqrt(f, sk2, lnw)
    muld(sk2, new Chunk(0.50), sk1, lnw)
    f.exponent = -1
    f.mantissa(0) = (0.50 * 1.6777216e7).toFloat
    sub(sk2, f, sk4, lnw)
    k = 1
    while (k <= mq) {
      add(sk0, sk1, sk2, lnw)
      mul(sk0, sk1, sk3, lnw)
      mpsqrt(sk3, sk1, lnw)
      muld(sk2, new Chunk(0.50), sk0, lnw)
      sub(sk0, sk1, sk2, lnw)
      mul(sk2, sk2, sk3, lnw)
      t1 = Math.pow(2.0, k)
      muld(sk3, new Chunk(t1), sk2, lnw)
      sub(sk4, sk2, sk3, lnw)
      eq(sk3, sk4, lnw)
      k += 1
    }
    add(sk0, sk1, sk2, lnw)
    mul(sk2, sk2, sk3, lnw)
    mpdiv(sk3, sk4, sk2, lnw)
    eq(sk2, pi, lnw)
    round(pi, nws)
  }

  def mpfftcr(is: Int, 
      m: Int, 
      n: Int, 
      x: Array[Complex], 
      y: Array[Double]) {
    var k = 0
    val pointFive = new Complex(0.5)
    val zeroOne = new Complex(0.0, 1.0)
    val mx = uu1(0).real().toInt
    if ((is != 1 && is != -1) || m < 3 || m > mx) throw new ArithmeticException("mpfftcr: Either the UU arrays have not been initialized or one of the input parameters is invalid: " + 
      is + 
      "\t" + 
      m + 
      "\t" + 
      mx)
    val dc1 = Array.ofDim[Complex](n >> 1)
    var a1: Complex = null
    var a2: Complex = null
    var x1: Complex = null
    var x2: Complex = null
    val n1 = (Math.pow(2, (m >> 1))).toInt
    val n2 = n >> 1
    val n4 = n >> 2
    dc1(0) = pointFive.multiply(new Complex((x(0).add(x(n2))).real(), (x(0).subtract(x(n2))).real()))
    dc1(n4) = if ((is == 1)) x(n4).conjg() else x(n4).clone().asInstanceOf[Complex]
    val ku = n2
    if (is == 1) {
      k = 1
      while (k < n4) {
        x1 = x(k)
        x2 = x(n2 - k).conjg()
        a1 = x1.add(x2)
        a2 = zeroOne.multiply(uu1(k + ku)).multiply(x1.subtract(x2))
        dc1(k) = pointFive.multiply(a1.add(a2))
        dc1(n2 - k) = pointFive.multiply((a1.subtract(a2)).conjg())
        k += 1
      }
    } else {
      k = 1
      while (k < n4) {
        x1 = x(k)
        x2 = (x(n2 - k)).conjg()
        a1 = x1.add(x2)
        a2 = zeroOne.multiply(uu1(k + ku).conjg()).multiply(x1.subtract(x2))
        dc1(k) = pointFive.multiply(a1.add(a2))
        dc1(n2 - k) = pointFive.multiply((a1.subtract(a2)).conjg())
        k += 1
      }
    }
    mpfft1(is, m - 1, n1, n2 / n1, dc1, x)
    k = 0
    while (k < n >> 1) {
      y(k << 1) = dc1(k).real()
      y((k << 1) + 1) = dc1(k).aimag()
      k += 1
    }
  }

  def mpfftrc(is: Int, 
      m: Int, 
      n: Int, 
      x: Array[Double], 
      y: Array[Complex]) {
    var k = 0
    val mx = uu1(0).real().toInt
    if ((is != 1 && is != -1) || m < 3 || m > mx) throw new ArithmeticException("mpfftrc: Either the UU arrays have not been initialized or one of the input parameters is invalid: " + 
      is + 
      "\t" + 
      m + 
      "\t" + 
      mx)
    var dc1 = Array.ofDim[Complex](n >> 1)
    var a1: Complex = null
    var a2: Complex = null
    var z1: Complex = null
    var z2: Complex = null
    val n1 = Math.pow(2, (m >> 1)).toInt
    val n2 = n >> 1
    val n4 = n >> 2
    k = 0
    while (k < n2) {dc1(k) = new Complex(x(k << 1), x((k << 1) + 1))k += 1
    }
    mpfft1(is, m - 1, n1, n2 / n1, dc1, y)
    y(0) = new Complex(2.0 * (dc1(0).real() + dc1(0).aimag()), 0.0)
    y(n4) = (if ((is == 1)) (dc1(n4)) else ((dc1(n4).conjg())))
      .multiply(new Complex(2.0))
    y(n2) = new Complex(2.0 * (dc1(0).real() - dc1(0).aimag()), 0.0)
    val ku = n2
    val zeroMinOne = new Complex(0.0, -1.0)
    if (is == 1) {
      k = 1
      while (k < n4) {
        z1 = dc1(k)
        z2 = dc1(n2 - k).conjg()
        a1 = z1.add(z2)
        a2 = zeroMinOne.multiply(uu1(k + ku)).multiply(z1.subtract(z2))
        y(k) = a1.add(a2)
        y(n2 - k) = a1.subtract(a2).conjg()
        k += 1
      }
    } else {
      k = 1
      while (k < n4) {
        z1 = dc1(k)
        z2 = dc1(n2 - k).conjg()
        a1 = z1.add(z2)
        a2 = zeroMinOne.multiply(uu1(k + ku).conjg()).multiply(z1.subtract(z2))
        y(k) = a1.add(a2)
        y(n2 - k) = a1.subtract(a2).conjg()
        k += 1
      }
    }
  }

  def mpfft1(is: Int, 
      m: Int, 
      n1: Int, 
      n2: Int, 
      x: Array[Complex], 
      y: Array[Complex]) {
    var i = 0
    var j = 0
    var k = 0
    val z1 = Array.ofDim[Complex](18, n1)
    val z2 = Array.ofDim[Complex](18, n1)
    val yrow = n2 + 2
    val m1 = (m + 1) >> 1
    val m2 = m - m1
    val nr1 = Math.min(n1, 16)
    val nr2 = Math.min(n2, 16)
    val ku = uu2(m - 1).real().toInt
    i = 0
    while (i < n1) {
      k = 0
      while (k < nr1) {
        j = 0
        while (j < n2) {z1(k)(j) = x(j * n1 + i + k)j += 1
        }
        k += 1
      }
      mpfft2(is, nr1, m2, n2, z1, z2)
      val iu = i + ku - n1 - 1
      if (is == 1) {
        k = 0
        while (k < nr1) {
          j = 0
          while (j < n2) {y((i + k) * yrow + j) = uu2(iu + k + (j + 1) * n1).multiply(z1(k)(j))j += 1
          }
          k += 1
        }
      } else {
        k = 0
        while (k < nr1) {
          j = 0
          while (j < n2) {y((i + k) * yrow + j) = uu2(iu + k + (j + 1) * n1).conjg().multiply(z1(k)(j))j += 1
          }
          k += 1
        }
      }
      i += nr1
    }
    i = 0
    while (i < n2) {
      k = 0
      while (k < nr2) {
        j = 0
        while (j < n1) {z2(k)(j) = y(j * yrow + i + k)j += 1
        }
        k += 1
      }
      mpfft2(is, nr2, m1, n1, z2, z1)
      if ((m % 2) == 0) {
        k = 0
        while (k < nr2) {
          j = 0
          while (j < n1) {x(i + k + j * n1) = z2(k)(j)j += 1
          }
          k += 1
        }
      } else {
        var j2 = 0
        j = 0
        while (j < n1 >> 1) {
          j2 = (j << 1)
          k = 0
          while (k < nr2) {
            x(i + k + j * n1) = z2(k)(j2)
            x(i + k + n2 + n1 * j) = z2(k)(j2 + 1)
            k += 1
          }
          j += 1
        }
      }
      i += nr2
    }
  }

  def mpfft2(is: Int, 
      ns: Int, 
      m: Int, 
      n: Int, 
      x: Array[Array[Complex]], 
      y: Array[Array[Complex]]) {
    var l = 0
    var j = 0
    var i = 0
    l = 1
    while (l <= m) {
      mpfft3(is, l, ns, m, n, x, y)
      if (l == m) {
        i = 0
        while (i < ns) {
          j = 0
          while (j < n) {x(i)(j) = y(i)(j)j += 1
          }
          i += 1
        }
        return
      }
      mpfft3(is, l + 1, ns, m, n, y, x)
      l += 2
    }
  }

  def mpfft3(is: Int, 
      l: Int, 
      ns: Int, 
      m: Int, 
      n: Int, 
      x: Array[Array[Complex]], 
      y: Array[Array[Complex]]) {
    var u1: Complex = null
    var x1: Complex = null
    var x2: Complex = null
    var i = 0
    var j = 0
    var k = 0
    val n1 = n >> 1
    val lk = Math.pow(2, (l - 1)).toInt
    val li = Math.pow(2, (m - l)).toInt
    val lj = (lk << 1)
    val ku = li
    var i11 = 0
    var i12 = 0
    var i21 = 0
    var i22 = 0
    i = 0
    while (i <= li - 1) {
      i11 = i * lk + 1
      i12 = i11 + n1
      i21 = i * lj + 1
      i22 = i21 + lk
      u1 = if ((is == 1)) uu1(i + ku) else (uu1(i + ku).conjg())
      k = -1
      while (k < lk - 1) {
        j = 0
        while (j < ns) {
          x1 = x(j)(i11 + k)
          x2 = x(j)(i12 + k)
          y(j)(i21 + k) = x1.add(x2)
          y(j)(i22 + k) = u1.multiply(x1.subtract(x2))
          j += 1
        }
        k += 1
      }
      i += 1
    }
  }

  def mplconv(iq: Int, 
      n: Int, 
      nsq: Int, 
      a: Array[Double], 
      b: Array[Double], 
      c: Array[Double]) {
    var i = 0
    var j = 0
    var k = 0
    var an = 0.0
    var t1 = 0.0
    var t2 = 0.0
    val ncr1 = Math.pow(2, (pointer - 1)).toInt
    var n1 = 0
    var n2 = 0
    if (n < ncr1) iq match {
      case 1 => k = 0
      while (k < (n << 1)) {
        t1 = 0.0
        n1 = Math.max(k - n + 2, 1)
        n2 = Math.min(k + 1, n)
        j = n1 - 1
        while (j < n2) {t1 += a(j) * a(k - j)j += 1
        }
        c(k) = t1
        k += 1
      }
      case 2 => k = 0
      while (k < (n << 1)) {
        t1 = 0.0
        n1 = Math.max(k - n + 2, 1)
        n2 = Math.min(k + 1, n)
        j = n1 - 1
        while (j < n2) {t1 += a(j) * b(k - j)j += 1
        }
        c(k) = t1
        k += 1
      }
      case -1 => 
        k = 0
        while (k < n - 1) {c(k) = 0.0k += 1
        }
        k = n - 1
        while (k < (n << 1)) {
          t1 = 0.0
          n1 = k - n + 2
          n2 = n
          j = n1 - 1
          while (j < n2) {t1 += a(j) * a(k - j)j += 1
          }
          c(k) = t1
          k += 1
        }

      case -2 => 
        k = 0
        while (k < n - 1) {c(k) = 0.0k += 1
        }
        k = n - 1
        while (k < (n << 1)) {
          t1 = 0.0
          n1 = k - n + 2
          n2 = n
          j = n1 - 1
          while (j < n2) {t1 += a(j) * b(k - j)j += 1
          }
          c(k) = t1
          k += 1
        }

    }
    val d1 = Array.ofDim[Double](3 * n + 2)
    val d2 = Array.ofDim[Double](3 * n + 2)
    val d3 = Array.ofDim[Double](3 * n + 2)
    val dc1 = Array.ofDim[Complex](3 * (n >> 1) + (nsq << 1) + 3)
    val dc2 = Array.ofDim[Complex](3 * (n >> 1) + (nsq << 1) + 3)
    t1 = 0.75 * n
    val m1 = (CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14).toInt
    n1 = (Math.pow(2, m1)).toInt
    var m2 = m1 + 1
    n2 = n1 << 1
    val n4 = n2 << 1
    val nm = Math.min((n << 1), n2)
    if (Math.abs(iq) == 1) {
      i = 0
      while (i < n) {d1(i) = a(i)i += 1
      }
      i = n
      while (i < n2) {d1(i) = 0.0i += 1
      }
      mpfftrc(1, m2, n2, d1, dc1)
      i = 0
      while (i < n1 + 1) {dc1(i) = dc1(i).multiply(dc1(i))i += 1
      }
    } else {
      i = 0
      while (i < n) {
        d1(i) = a(i)
        d2(i) = b(i)
        i += 1
      }
      i = n
      while (i < n2) {
        d1(i) = 0.0
        d2(i) = 0.0
        i += 1
      }
      mpfftrc(1, m2, n2, d1, dc1)
      mpfftrc(1, m2, n2, d2, dc2)
      i = 0
      while (i <= n1) {dc1(i) = dc1(i).multiply(dc2(i))i += 1
      }
    }
    mpfftcr(-1, m2, n2, dc1, d3)
    an = 1.0 / n4
    i = 0
    while (i < nm) {
      t1 = an * d3(i)
      t2 = nint(t1)
      c(i) = t2
      i += 1
    }
    val skip = true
    if (!skip) {
      var i1 = 0
      t1 = 0.0
      i = 0
      while (i < nm) {
        if (d1(i) > t1) {
          i1 = i
          t1 = d1(i)
        }
        i += 1
      }
      if (t1 > 0.438) {
        t2 = an * d1(i1)
        val i2 = (CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14).toInt
        val i3 = (CL2 * Math.log(t2) + 1.0 + 5.6843418860808015e-14).toInt
        val i4 = 53 + i2 - i3
        val i5 = (t1 * Math.pow(2, i4) + 5.6843418860808015e-14).toInt
        throw new ArithmeticException("mplconv: Excessive FFT roundoff error --> \t" + i1 + 
          "\t" + 
          t1 + 
          "\t" + 
          i4 + 
          "\t" + 
          i5)
      }
    }
    var m = 0
    var m21 = 0
    var ms = 0
    if (n > n1) {
      m = n - n1
      m2 = (m << 1)
      m21 = m2 - 1
      ms = (Math.sqrt(3.0 * m21) + 5.6843418860808015e-14).toInt
      k = n1 - m + 1
      if (Math.abs(iq) == 1) {
        i = 0
        while (i < m21) {d1(i) = a(k + i)i += 1
        }
        mplconv(-1, m21, ms, d1, d2, d3)
      } else {
        i = 0
        while (i < m21) {
          d1(i) = a(k + i)
          d2(i) = b(k + i)
          i += 1
        }
        mplconv(-2, m21, ms, d1, d2, d3)
      }
      var ii: Int = 0
      i = 0
      while (i < m2) {
        ii = i + m2 - 2
        c(i) -= d3(ii)
        c(i + n2) = d3(ii)
        i += 1
      }
    }
  }

  def mpcbrx(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    var k = 0
    var t1 = 0.0
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val na = Math.min(a.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (na == 0) {
      zero(b)
      return
    }
    if (a.sign == false) throw new ArithmeticException("mpcbrx: Argument is negative --> " + a)
    if (lnw <= ncr) {
      cbrt(a, b, lnw)
      return
    }
    val nws = lnw
    t1 = lnw
    val mq = (CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14).toInt
    _sq(a, sk0, lnw)
    lnw = ncr + 1
    cbrt(a, sk1, lnw)
    mpdiv(sk1, a, b, lnw)
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    var iq = 0
    var nw1 = 0
    var nw2 = 0
    k = pointer + 1
    while (k <= mq - 1) {
      nw1 = lnw
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      nw2 = lnw
      var cont = true
      while (cont) {
        _sq(b, sk1, lnw)
        _mul(b, sk1, sk2, lnw)
        _mul(sk0, sk2, sk1, lnw)
        sub(f, sk1, sk2, lnw)
        lnw = nw1
        _mul(b, sk2, sk1, lnw)
        mpdivd(sk1, new Chunk(3.0), sk2, lnw)
        lnw = nw2
        add(b, sk2, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else cont = false
      }
      k += 1
    }
    _mul(a, b, sk0, lnw)
    nw1 = lnw
    lnw = Math.min((lnw << 1) - 2, nws) + 1
    nw2 = lnw
    _sq(sk0, sk1, lnw)
    _mul(sk0, sk1, sk2, lnw)
    sub(a, sk2, sk1, lnw)
    lnw = nw1
    _mul(sk1, b, sk2, lnw)
    mpdivd(sk2, new Chunk(3.0), sk1, lnw)
    lnw = nw2
    add(sk0, sk1, sk2, lnw)
    eq(sk2, b, lnw)
    round(b, nws)
  }

  def _div(a: PrecisionNumber, 
      b: PrecisionNumber, 
      c: PrecisionNumber, 
      lnw: Int) {
    var k = 0
    var t1 = 0.0
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val nb = Math.min(b.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (nb == 0) throw new ArithmeticException("_div: Divisor is zero")
    if (nb <= ncr) {
      mpdiv(a, b, c, lnw)
      return
    }
    val nws = lnw
    t1 = lnw
    val mq = (CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14).toInt
    lnw = ncr + 1
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    mpdiv(f, b, c, lnw)
    var iq = 0
    var nw1 = 0
    var nw2 = 0
    k = pointer + 1
    while (k <= mq - 1) {
      nw1 = lnw
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      nw2 = lnw
      var cont = true
      while (cont) {
        _mul(b, c, sk0, lnw)
        sub(f, sk0, sk1, lnw)
        lnw = nw1
        _mul(c, sk1, sk0, lnw)
        lnw = nw2
        add(c, sk0, sk1, lnw)
        eq(sk1, c, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else cont = false
      }
      k += 1
    }
    _mul(a, c, sk0, lnw)
    nw1 = lnw
    lnw = Math.min((lnw << 1) - 2, nws) + 1
    nw2 = lnw
    _mul(sk0, b, sk1, lnw)
    sub(a, sk1, sk2, lnw)
    lnw = nw1
    _mul(sk2, c, sk1, lnw)
    lnw = nw2
    add(sk0, sk1, sk2, lnw)
    eq(sk2, c, lnw)
    round(c, nws)
  }

  def _mul(a: PrecisionNumber, 
      b: PrecisionNumber, 
      c: PrecisionNumber, 
      lnw: Int) {
    var t1 = 0.0
    var t2 = 0.0
    var t3 = 0.0
    var t4 = 0.0
    var i = 0
    val na = Math.min(a.number_words, lnw)
    val nb = Math.min(b.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (na <= ncr || nb <= ncr) {
      mul(a, b, c, lnw)
      return
    }
    val d1 = Array.ofDim[Double]((lnw + 2) << 1)
    val d2 = Array.ofDim[Double]((lnw + 2) << 1)
    val d3 = Array.ofDim[Double]((lnw + 3) << 2)
    var i2 = 0
    i = 0
    while (i < na) {
      i2 = i << 1
      t1 = a.mantissa(i)
      t2 = (2.44140625e-4 * t1).toInt
      d1(i2) = t2
      d1(i2 + 1) = t1 - 4096.0 * t2
      i += 1
    }
    i = na << 1
    while (i < nb << 1) {d1(i) = 0.0i += 1
    }
    i = 0
    while (i < nb) {
      i2 = i << 1
      t1 = b.mantissa(i)
      t2 = (2.44140625e-4 * t1).toInt
      d2(i2) = t2
      d2(i2 + 1) = t1 - 4096.0 * t2
      i += 1
    }
    i = (nb << 1)
    while (i < (na << 1)) {d2(i) = 0.0i += 1
    }
    val nn = Math.max(na, nb) << 1
    val nx = (Math.sqrt(3.0 * nn) + 5.6843418860808015e-14).toInt
    mplconv(2, nn, nx, d1, d2, d3)
    val nc = Math.min(na + nb, lnw)
    val nc1 = Math.min(lnw + 1, na + nb - 1)
    d1(0) = fSign(nc, if ((!(a.sign ^ b.sign))) 1.0 else -1.0)
    d1(1) = a.exponent + b.exponent + 1
    d1(2) = d3(0)
    d1(nc + 2) = 0.0
    d1(nc + 3) = 0.0
    i = 1
    while (i <= nc1) {
      i2 = i << 1
      t3 = d3(i2 - 1)
      t4 = d3(i2)
      t1 = (5.9604644775390625e-8 * t3).toInt
      t2 = t3 - 1.6777216e7 * t1
      t3 = (5.9604644775390625e-8 * t4).toInt
      t4 -= 1.6777216e7 * t3
      d1(i + 2) = 4096.0 * t2 + t4
      d1(i + 1) += 4096.0 * t1 + t3
      i += 1
    }
    mpnorm(d1, c, lnw)
  }

  def _npw(a: PrecisionNumber, 
      n: Int, 
      b: PrecisionNumber, 
      lnw: Int) {
    var j = 0
    var t1 = 0.0
    val nw2 = lnw + 2
    val f1 = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw2, false)
    val sk1 = new PrecisionNumber(nw2, false)
    val ncr = (Math.pow(2, pointer)).toInt
    val na = Math.min(a.number_words, lnw)
    if (na <= ncr && n >= 0 && n <= 4) {
      mpnpwr(a, n, b, lnw)
      return
    }
    if (na == 0) {
      if (n >= 0) {
        zero(b)
        return
      } else throw new ArithmeticException("_npw: Argument is zero and n is negative or zero")
    }
    val nn = Math.abs(n)
    f1.sign = true
    f1.number_words = 1
    f1.exponent = 0
    f1.mantissa(0) = 1
    f1.mantissa(1) = 0
    var skip = false
    nn match {
      case 0 => 
        eq(f1, b, lnw)
        return

      case 1 => 
        eq(a, b, lnw)
        skip = true

      case 2 => 
        _sq(a, b, lnw)
        skip = true

    }
    if (!skip) {
      t1 = nn
      val mn = (CL2 * Math.log(t1) + 1.0 + 5.6843418860808015e-14).toInt
      eq(f1, b, lnw)
      eq(a, sk0, lnw)
      var kn = nn
      j = 1
      while (j <= mn) {
        val kk = kn >> 1
        if (kn != kk << 1) {
          _mul(b, sk0, sk1, lnw)
          eq(sk1, b, lnw)
        }
        kn = kk
        if (j < mn) {
          _sq(sk0, sk1, lnw)
          eq(sk1, sk0, lnw)
        }
        j += 1
      }
    }
    if (n < 0) {
      _div(f1, b, sk0, lnw)
      eq(sk0, b, lnw)
    }
  }

  def _nrt(a: PrecisionNumber, 
      n: Int, 
      b: PrecisionNumber, 
      lnw: Int) {
    var k = 0
    var t2 = 0.0
    var tn = 0.0
    val nw3 = lnw + 3
    val f1 = new PrecisionNumber(6, false)
    val f2 = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val sk3 = new PrecisionNumber(nw3, false)
    val ncr = (Math.pow(2, pointer)).toInt
    val na = Math.min(a.number_words, lnw)
    if (na == 0) {
      zero(b)
      return
    }
    if (!a.sign) throw new ArithmeticException("_nrt: Argument is negative --> " + a)
    if (n <= 0 || n > N30) throw new ArithmeticException("_nrt: Improper value of N --> " + n)
    if (lnw <= ncr) {
      mpnrt(a, n, b, lnw)
      return
    }
    n match {
      case 1 => 
        eq(a, b, lnw)
        return

      case 2 => 
        _sqr(a, b, lnw)
        return

      case 3 => 
        mpcbrx(a, b, lnw)
        return

    }
    val nws = lnw
    f1.number_words = 1
    f1.sign = true
    f1.exponent = 0
    f1.mantissa(0) = 1
    f1.mantissa(1) = 0
    val t1 = new Chunk()
    t1.a = lnw
    val mq = (CL2 * Math.log(t1.a) + 1.0 - 5.6843418860808015e-14).toInt
    sub(a, f1, sk0, lnw)
    if (sk0.number_words == 0) {
      eq(f1, b, lnw)
      return
    }
    mdc(sk0, t1)
    var n2 = (CL2 * Math.log(Math.abs(t1.a))).toInt
    t1.a *= Math.pow(0.5, n2)
    t1.n += n2
    if (t1.n <= -30) {
      t2 = n
      n2 = (CL2 * Math.log(t2) + 1.0 + 5.6843418860808015e-14).toInt
      val n3 = -24 * lnw / t1.n
      if (n3 < 1.25 * n2) {
        mpdivd(sk0, new Chunk(t2), sk1, lnw)
        add(f1, sk1, sk2, lnw)
        k = 0
        do {
          k += 1
          t1.a = 1 - k * n
          t2 = (k + 1) * n
          muld(sk1, new Chunk(t1.a), sk3, lnw)
          mpdivd(sk3, new Chunk(t2), sk1, lnw)
          _mul(sk0, sk1, sk3, lnw)
          eq(sk3, sk1, lnw)
          add(sk1, sk2, sk3, lnw)
          eq(sk3, sk2, lnw)
        } while (sk1.number_words != 0 && sk1.exponent >= -lnw);
        eq(sk2, b, lnw)
        round(b, nws)
        return
      }
    }
    lnw = ncr + 1
    mpnrt(a, n, sk0, lnw)
    mpdiv(f1, sk0, b, lnw)
    tn = n
    val dpn = new Chunk(n, 0)
    dmc(dpn, f2)
    var iq = 0
    var nw1 = 0
    var nw2 = 0
    k = pointer + 1
    while (k <= mq) {
      nw1 = lnw
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      nw2 = lnw
      var loop = true
      while (loop) {
        _npw(b, n, sk0, lnw)
        _mul(a, sk0, sk1, lnw)
        sub(f1, sk1, sk0, lnw)
        lnw = nw1
        _mul(b, sk0, sk1, lnw)
        mpdivd(sk1, new Chunk(tn), sk0, lnw)
        lnw = nw2
        add(b, sk0, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else loop = false
      }
      k += 1
    }
    _div(f1, b, sk0, lnw)
    eq(sk0, b, lnw)
    round(b, nws)
  }

  def _sqr(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    var k = 0
    val nw3 = lnw + 3
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw3, false)
    val sk1 = new PrecisionNumber(nw3, false)
    val sk2 = new PrecisionNumber(nw3, false)
    val na = Math.min(a.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (na == 0) {
      zero(b)
      return
    }
    if (!a.sign) throw new ArithmeticException("_sqr: Argument is negative --> " + a)
    if (lnw <= ncr) {
      mpsqrt(a, b, lnw)
      return
    }
    val nws = lnw
    val t1 = lnw
    val mq = (CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14).toInt
    lnw = ncr + 1
    mpsqrt(a, sk0, lnw)
    mpdiv(sk0, a, b, lnw)
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    var iq = 0
    var nw1 = 0
    var nw2 = 0
    k = pointer + 1
    while (k <= mq - 1) {
      nw1 = lnw
      lnw = Math.min((lnw << 1) - 2, nws) + 1
      nw2 = lnw
      var stop = false
      while (!stop) {
        _sq(b, sk0, lnw)
        _mul(a, sk0, sk1, lnw)
        sub(f, sk1, sk0, lnw)
        lnw = nw1
        _mul(b, sk0, sk1, lnw)
        muld(sk1, new Chunk(0.50), sk0, lnw)
        lnw = nw2
        add(b, sk0, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - NIT && iq == 0) iq = 1 else stop = true
      }
      k += 1
    }
    _mul(a, b, sk0, lnw)
    nw1 = lnw
    lnw = Math.min((lnw << 1) - 2, nws) + 1
    nw2 = lnw
    _sq(sk0, sk1, lnw)
    sub(a, sk1, sk2, lnw)
    lnw = nw1
    _mul(sk2, b, sk1, lnw)
    muld(sk1, new Chunk(0.50), sk2, lnw)
    lnw = nw2
    add(sk0, sk2, sk1, lnw)
    eq(sk1, b, lnw)
    round(b, nws)
  }

  def _sq(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    var t1 = 0.0
    var t2 = 0.0
    var t3 = 0.0
    var t4 = 0.0
    var i = 0
    val na = Math.min(a.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (na == 0) {
      zero(b)
      return
    }
    if (na <= ncr) {
      mul(a, a, b, lnw)
      return
    }
    val d1 = Array.ofDim[Double]((lnw + 2) << 1)
    val d2 = Array.ofDim[Double]((lnw + 2) << 2)
    var i2 = 0
    i = 0
    while (i < na) {
      i2 = i << 1
      t1 = a.mantissa(i)
      t2 = (2.44140625e-4 * t1).toInt
      d1(i2) = t2
      d1(i2 + 1) = t1 - 4096.0 * t2
      i += 1
    }
    val nn = (na << 1)
    val nx = (Math.sqrt(3.0 * nn) + 5.6843418860808015e-14).toInt
    mplconv(1, nn, nx, d1, d1, d2)
    val nc = Math.min((na << 1), lnw)
    val nc1 = Math.min(lnw + 1, (na << 1) - 1)
    d1(0) = nc
    d1(1) = (a.exponent << 1) + 1
    d1(2) = d2(0)
    d1(nc + 2) = 0.0
    d1(nc + 3) = 0.0
    i = 1
    while (i <= nc1) {
      i2 = i << 1
      t3 = d2(i2 - 1)
      t4 = d2(i2)
      t1 = (5.9604644775390625e-8 * t3).toInt
      t2 = t3 - 1.6777216e7 * t1
      t3 = (5.9604644775390625e-8 * t4).toInt
      t4 -= 1.6777216e7 * t3
      d1(i + 2) = 4096.0 * t2 + t4
      d1(i + 1) += 4096.0 * t1 + t3
      i += 1
    }
    mpnorm(d1, b, lnw)
  }

  private def mpagmx(a: PrecisionNumber, b: PrecisionNumber, lnw: Int) {
    val nw2 = lnw + 2
    val sk0 = new PrecisionNumber(nw2, false)
    val sk1 = new PrecisionNumber(nw2, false)
    var l1 = 0
    var s1 = 0
    val dpe1 = new Chunk(0.50, 0)
    do {
      l1 += 1
      if (l1 == 50) throw new ArithmeticException("mpagmx: Iteration limit exceeded.")
      s1 = sk0.exponent
      add(a, b, sk0, lnw)
      muld(sk0, dpe1, sk1, lnw)
      _mul(a, b, sk0, lnw)
      _sqr(sk0, b, lnw)
      eq(sk1, a, lnw)
      sub(a, b, sk0, lnw)
    } while (sk0.number_words != 0. && (sk0.exponent < s1 || sk0.exponent >= -2));
  }

  def _cosh(a: PrecisionNumber, 
      pi: PrecisionNumber, 
      al2: PrecisionNumber, 
      x: PrecisionNumber, 
      y: PrecisionNumber, 
      lnw: Int) {
    val nw2 = lnw + 2
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw2, false)
    val sk1 = new PrecisionNumber(nw2, false)
    val sk2 = new PrecisionNumber(nw2, false)
    f.sign = true
    f.number_words = 1
    f.exponent = 0
    f.mantissa(0) = 1
    f.mantissa(1) = 0
    val dpe1 = new Chunk(0.5)
    _exp(a, pi, al2, sk0, lnw)
    _div(f, sk0, sk1, lnw)
    add(sk0, sk1, sk2, lnw)
    muld(sk2, dpe1, x, lnw)
    sub(sk0, sk1, sk2, lnw)
    muld(sk2, dpe1, y, lnw)
  }

  def _exp(a: PrecisionNumber, 
      pi: PrecisionNumber, 
      al2: PrecisionNumber, 
      b: PrecisionNumber, 
      lnw: Int) {
    var k = 0
    val nit = 1
    val nw2 = lnw + 2
    val f1 = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw2, false)
    val sk1 = new PrecisionNumber(nw2, false)
    val sk2 = new PrecisionNumber(nw2, false)
    val ncr = (Math.pow(2, pointer)).toInt
    val t1 = new Chunk()
    mdc(a, t1)
    t1.a *= Math.pow(2.0, t1.n)
    if (lnw <= ncr) {
      mpexp(a, al2, b, lnw)
      return
    }
    val t2 = new Chunk()
    mdc(al2, t2)
    if (t2.n != -24 || 
      Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15) throw new ArithmeticException("_exp: LOG(2) must be precomputed.")
    mdc(pi, t2)
    if (t2.n != 0 || Math.abs(t2.a - Math.PI) > 3.552713678800501e-15) throw new ArithmeticException("_exp: PI must be precomputed.")
    if (t1.a >= 1e9) {
      if (t1.a > 0.0) throw new ArithmeticException("_exp: Argument is too large --> " + t1.a + " x 10 ^" + 
        t1.n) else {
        zero(b)
        return
      }
    }
    val nws = lnw
    f1.sign = true
    f1.number_words = 1
    f1.exponent = 0
    f1.mantissa(0) = 1
    f1.mantissa(1) = 0
    t2.a = nws
    val mq = (CL2 * Math.log(t2.a) + 1.0 - 5.6843418860808015e-14).toInt
    add(a, f1, sk0, lnw)
    lnw = ncr
    mpexp(a, al2, b, lnw)
    var iq = 0
    k = pointer + 1
    while (k <= mq) {
      lnw = Math.min(lnw << 1, nws)
      var cont = true
      while (cont) {
        _log(b, pi, al2, sk1, lnw)
        sub(sk0, sk1, sk2, lnw)
        _mul(b, sk2, sk1, lnw)
        eq(sk1, b, lnw)
        if (k == mq - nit && iq == 0) iq = 1 else cont = false
      }
      k += 1
    }
  }

  def _log(a: PrecisionNumber, 
      pi: PrecisionNumber, 
      al2: PrecisionNumber, 
      b: PrecisionNumber, 
      lnw: Int) {
    val mzl = -5
    var st = 0.0
    var tn = 0.0
    val nw2 = lnw + 2
    val f1 = new PrecisionNumber(6, false)
    val f4 = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw2, false)
    val sk1 = new PrecisionNumber(nw2, false)
    val sk2 = new PrecisionNumber(nw2, false)
    val sk3 = new PrecisionNumber(nw2, false)
    val na = Math.min(a.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (lnw <= ncr) {
      log(a, al2, b, lnw)
      return
    }
    if (!a.sign || na == 0) throw new ArithmeticException("_log: Argument is less than or equal to zero -->" + a)
    val t1 = new Chunk()
    mdc(pi, t1)
    if (t1.n != 0 || Math.abs(t1.a - CPI) > 3.552713678800501e-15) throw new ArithmeticException("_log: PI must be precomputed.")
    val t2 = new Chunk()
    var it2 = 0
    if (a.number_words != 1 || a.exponent != 0 || a.mantissa(0) != 2 || 
      !a.sign) {
      it2 = 0
      mdc(al2, t2)
      if (t2.n != -24 || 
        Math.abs(t2.a * Math.pow(0.50, 24) - ALT) > 3.552713678800501e-15) throw new ArithmeticException("_log: LOG(2) must be precomputed.")
    } else it2 = 1
    f1.number_words = 1
    f1.sign = true
    f1.exponent = 0
    f1.mantissa(0) = 1
    f1.mantissa(1) = 0
    f4.number_words = 1
    f4.sign = true
    f4.exponent = 0
    f4.mantissa(0) = 4
    f4.mantissa(1) = 0
    sub(a, f1, sk0, lnw)
    if (sk0.number_words == 0) {
      zero(b)
      return
    } else if (sk0.exponent <= mzl) {
      eq(sk0, sk1, lnw)
      eq(sk1, sk2, lnw)
      var i1 = 1
      var is = 1
      val tl = sk0.exponent - lnw - 1
      do {
        i1 += 1
        is = -is
        st = is * i1
        _mul(sk1, sk2, sk3, lnw)
        mpdivd(sk3, new Chunk(st), sk2, lnw)
        add(sk0, sk2, sk3, lnw)
        eq(sk3, sk0, lnw)
      } while (sk2.exponent >= tl);
      eq(sk0, b, lnw)
      return
    }
    mdc(a, t1)
    t2.n = 12 * lnw + 48 - t1.n
    tn = t2.n
    t2.a = 1.0
    if (it2 == 1) dmc(t2, sk0) else muld(a, t2, sk0, lnw)
    eq(f1, sk1, lnw)
    _div(f4, sk0, sk2, lnw)
    mpagmx(sk1, sk2, lnw)
    muld(sk1, new Chunk(2.0), sk0, lnw)
    _div(pi, sk0, sk1, lnw)
    if (it2 == 1) mpdivd(sk1, new Chunk(tn), sk0, lnw) else {
      muld(al2, new Chunk(tn), sk2, lnw)
      sub(sk1, sk2, sk0, lnw)
    }
    eq(sk0, b, lnw)
  }

  def _pi(pi: PrecisionNumber, lnw: Int) {
    var k = 0
    var t1 = 0.0
    val nw2 = lnw + 2
    val f = new PrecisionNumber(6, false)
    val sk0 = new PrecisionNumber(nw2, false)
    val sk1 = new PrecisionNumber(nw2, false)
    val sk2 = new PrecisionNumber(nw2, false)
    val sk3 = new PrecisionNumber(nw2, false)
    val sk4 = new PrecisionNumber(nw2, false)
    val ncr = (Math.pow(2, pointer)).toInt
    if (lnw <= ncr) {
      mppi(pi, lnw)
      return
    }
    t1 = lnw * log10(1.6777216e7)
    val mq = (CL2 * (Math.log(t1) - 1.0) + 1.0).toInt
    sk0.number_words = 1
    sk0.sign = true
    sk0.exponent = 0
    sk0.mantissa(0) = 1
    f.number_words = 1
    f.sign = true
    f.exponent = 0
    f.mantissa(0) = 2
    f.mantissa(1) = 0
    _sqr(f, sk2, lnw)
    muld(sk2, new Chunk(0.50), sk1, lnw)
    f.exponent = -1
    f.mantissa(0) = (0.50 * 1.6777216e7).toFloat
    sub(sk2, f, sk4, lnw)
    val dpe1 = new Chunk(0.50)
    k = 1
    while (k <= mq) {
      add(sk0, sk1, sk2, lnw)
      _mul(sk0, sk1, sk3, lnw)
      _sqr(sk3, sk1, lnw)
      muld(sk2, dpe1, sk0, lnw)
      sub(sk0, sk1, sk2, lnw)
      _sq(sk2, sk3, lnw)
      t1 = Math.pow(2.0, k)
      muld(sk3, new Chunk(t1), sk2, lnw)
      sub(sk4, sk2, sk3, lnw)
      eq(sk3, sk4, lnw)
      k += 1
    }
    add(sk0, sk1, sk2, lnw)
    _sq(sk2, sk3, lnw)
    _div(sk3, sk4, sk2, lnw)
    eq(sk2, pi, lnw)
  }

  def outx(a: PrecisionNumber, b: Array[Char], lnw: Int): Int = {
    var i = 0
    var n = 0
    val na = Math.min(a.number_words, lnw)
    val ncr = (Math.pow(2, pointer)).toInt
    if (na <= ncr) return mpoutc(a, b, lnw)
    var t1 = 0.0
    var t2 = 0.0
    var c1 = Array.ofDim[Char](17)
    val c2 = Array.ofDim[Char](17)
    val b1 = Array.ofDim[Char]((lnw << 3) + 31)
    val b2 = Array.ofDim[Char]((lnw << 3) + 31)
    val nw2 = lnw + 2
    val sk0 = new PrecisionNumber(nw2, false)
    val sk1 = new PrecisionNumber(nw2, false)
    val sk2 = new PrecisionNumber(nw2, false)
    val sk3 = new PrecisionNumber(nw2, false)
    val sk4 = new PrecisionNumber(nw2, false)
    t1 = a.mantissa(0) + 5.9604644775390625e-8 * a.mantissa(1) + 
      3.552713678800501e-15 * a.mantissa(2)
    t2 = log10(t1)
    val m1 = (Math.max(AL2 * 24 * (a.number_words - a.exponent) - t2, 0.0)).toInt
    val dpe1 = new Chunk(10, 0)
    dmc(dpe1, sk0)
    _npw(sk0, m1, sk2, lnw)
    _mul(a, sk2, sk1, lnw)
    sk1.sign = true
    val dpe2 = new Chunk()
    val dpe3 = new Chunk()
    mdc(sk1, dpe2)
    Chunk.dpdec(dpe2, dpe3)
    val m2 = dpe3.n >> 1
    _npw(sk0, m2, sk3, lnw)
    _div(sk1, sk3, sk0, lnw)
    infr(sk0, sk2, sk4, lnw)
    _mul(sk2, sk3, sk0, lnw)
    sub(sk1, sk0, sk3, lnw)
    val nws = lnw
    lnw = sk2.number_words + 1
    var nb1 = 0
    var nb2 = 0
    nb1 = outx(sk2, b1, lnw)
    lnw = sk3.number_words + 1
    nb2 = outx(sk3, b2, lnw)
    lnw = nws
    i = 0
    while (i < 17) {
      c1(i) = '\0'
      c2(i) = '\0'
      i += 1
    }
    i = 0
    while (i < 10) {
      c1(i) = b1(i + 4)
      c2(i) = b2(i + 4)
      i += 1
    }
    var tempStr = new String(c1)
    tempStr = tempStr.substring(0, tempStr.indexOf('\0'))
    val ie1 = java.lang.Integer.parseInt(tempStr)
    tempStr = new String(c2)
    tempStr = tempStr.substring(0, tempStr.indexOf('\0'))
    val ie2 = java.lang.Integer.parseInt(tempStr)
    val ie = ie1 + m2 - m1
    c1 = (new java.lang.Integer(ie)).toString.toCharArray()
    i = 0
    while (i < 4) {b(i) = b1(i)i += 1
    }
    while (i < 14 - c1.length) {b(i) = ' 'i += 1
    }
    val ii = 0
    while (i < 14) {b(i) = c1(ii += 1)i += 1
    }
    i = 14
    while (i < nb1) {b(i) = b1(i)i += 1
    }
    var i2 = 0
    val i1 = ie1 + m2 - ie2 + 19
    if (nb1 > i1) {
      i2 = java.lang.Integer.parseInt(new String(b, i1, 1))
      if (i2 >= 5) {
        var skip = false
        i = i1 - 1
        while (i >= 20) {
          if (b(i) != '9') {
            skip = true
            //break
          }
          b(i) = '0'
          i -= 1
        }
        if (!skip) throw new ArithmeticException("outx: Exceptional case -- contact author.")
        i2 = java.lang.Integer.parseInt(new String(b, i, 1))
        c1 = (new java.lang.Integer(i2 + 1)).toString.toCharArray()
        b(i) = c1(0)
      }
    } else if (nb1 < i1) i = nb1
    while (i < i1) {b(i) = '0'i += 1
    }
    b(i1) = b2(18)
    n = Math.min(i1 + nb2 - 19, (7.225 * lnw + 30).toInt)
    i = i1 + 1
    while (i < n) {b(i) = b2(i - i1 + 19)i += 1
    }
    if (!a.sign) b(17) = '-'
    b(n) = '\0'
    n
  }

}
