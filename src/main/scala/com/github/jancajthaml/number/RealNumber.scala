package com.github.jancajthaml.number

import ComplexNumber.{_cos, _ang}
import RealNumber._

trait RealNumber extends PreciseNumber {

  def assign(ja: PreciseNumber): RealNumber = {
    if (ja != this) eq(ja, this, Math.min(nw, this.maxnw - 1))
    this
  }

  def add(ja: RealNumber): RealNumber = {
    val res = new RealNumber()
    add(this, ja, res, nw)
    res
  }

  def subtract(ja: RealNumber): RealNumber = {
    val res = new RealNumber()
    sub(this, ja, res, nw)
    res
  }

  def negate(): RealNumber = {
    val res = new RealNumber()
    eq(this, res, nw)
    res.sign = !this.sign
    res
  }

  def multiply(ja: RealNumber): RealNumber = {
    val res = new RealNumber()
    _mul(this, ja, res, nw)
    res
  }

  def divide(ja: RealNumber): RealNumber = {
    val res = new RealNumber()
    _div(this, ja, res, nw)
    res
  }

  def abs(): RealNumber = {
    val res = new RealNumber()
    eq(this, res, nw)
    res.sign = true
    res
  }

  def max(i: RealNumber): RealNumber = {
    if ((compare(this, i, nw) >= 0)) this else i
  }

  def min(i: RealNumber): RealNumber = {
    if ((compare(this, i, nw) < 0)) this else i
  }

  def sign(i: PreciseNumber): RealNumber = {
    val res = new RealNumber()
    eq(this, res, nw)
    res.sign = i.sign
    res
  }

  def pow(exponent: PreciseNumber): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    val mpt2 = new PreciseNumber()
    _log(this, PI, LOG2, mpt1, nw)
    _mul(mpt1, exponent, mpt2, nw)
    _exp(mpt2, PI, LOG2, mpt1, nw)
    res
  }

  def pow(exponent: Int): RealNumber = {
    val res = new RealNumber()
    _npw(this, exponent, res, nw)
    res
  }

  def pow(exponent: Double): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    val mpt2 = new PreciseNumber()
    _log(this, PI, LOG2, mpt1, nw)
    muld(mpt1, new Chunk(exponent), mpt2, nw)
    _exp(mpt2, PI, LOG2, res, nw)
    res
  }

  def acos(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    val mpt2 = new PreciseNumber()
    val mpt3 = new PreciseNumber()
    dmc(new Chunk(1), mpt1)
    _mul(this, this, mpt2, nw)
    sub(mpt1, mpt2, mpt3, nw)
    _sqr(mpt3, mpt1, nw)
    _ang(this, mpt1, PI, res, nw)
    res
  }

  def aint(): RealNumber = {
    val res = new RealNumber()
    infr(this, res, new PreciseNumber(), nw)
    res
  }

  def anint(): RealNumber = {
    val res = new RealNumber()
    nint(this, res, nw)
    res
  }

  def asin(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    val mpt2 = new PreciseNumber()
    val mpt3 = new PreciseNumber()
    dmc(new Chunk(1), mpt1)
    _mul(this, this, mpt2, nw)
    sub(mpt1, mpt2, mpt3, nw)
    _sqr(mpt3, mpt1, nw)
    _ang(mpt1, this, PI, res, nw)
    res
  }

  def atan(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber(6, false)
    dmc(new Chunk(1), mpt1)
    _ang(this, mpt1, PI, res, nw)
    res
  }

  def atan2(i: RealNumber): RealNumber = {
    val res = new RealNumber()
    _ang(i, this, PI, res, nw)
    res
  }

  def cos(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    _cos(this, PI, res, mpt1, nw)
    res
  }

  def cosh(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    _cosh(this, PI, LOG2, res, mpt1, nw)
    res
  }

  def exp(): RealNumber = {
    val res = new RealNumber()
    _exp(this, PI, LOG2, res, nw)
    res
  }

  def log(): RealNumber = {
    val res = new RealNumber()
    _log(this, PI, LOG2, res, nw)
    res
  }

  def log10(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    _log(this, PI, LOG2, mpt1, nw)
    _div(mpt1, LOG10, res, nw)
    res
  }

  def csshf(cosh: RealNumber, sinh: RealNumber) {
    _cosh(this, PI, LOG2, cosh, sinh, nw)
  }

  def cssnf(cosine: RealNumber, sine: RealNumber) {
    _cos(this, PI, cosine, sine, nw)
  }

  def nrtf(ib: Int): RealNumber = {
    val res = new RealNumber()
    _nrt(this, ib, res, nw)
    res
  }

  def nint(): IntegerNumber = {
    val res = new IntegerNumber()
    nint(this, res, nw)
    res
  }

  def sin(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    _cos(this, PI, mpt1, res, nw)
    res
  }

  def sinh(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    _cosh(this, PI, LOG2, mpt1, res, nw)
    res
  }

  def sqrt(): RealNumber = {
    val res = new RealNumber()
    _sqr(this, res, nw)
    res
  }

  def tan(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    val mpt2 = new PreciseNumber()
    _cos(this, PI, mpt1, mpt2, nw)
    _div(mpt1, mpt2, res, nw)
    res
  }

  def tanh(): RealNumber = {
    val res = new RealNumber()
    val mpt1 = new PreciseNumber()
    val mpt2 = new PreciseNumber()
    _cosh(this, PI, PI, mpt1, mpt2, nw)
    _div(mpt1, mpt2, res, nw)
    res
  }

}

object RealNumber {

  var LOG2: RealNumber = new RealNumber(mp21, false)

  var LOG10: RealNumber = new RealNumber(mp21, false)

  var PI: RealNumber = new RealNumber(mp21, false)

  var EPSILON: RealNumber = new RealNumber(mp21, false)

  _pi(PI, nw + 1)

  val t2 = new PreciseNumber(6, false)

  dmc(new Chunk(2), t2)

  _log(t2, PI, LOG2, LOG2, nw + 1)

  dmc(new Chunk(10), t2)

  _log(t2, PI, LOG2, LOG10, nw + 1)

  dmc(new Chunk(10), t2)

  _npw(t2, ellog10, EPSILON, nw + 1)

  PI.number_words -= 1

  LOG2.number_words -= 1

  LOG10.number_words -= 1

  EPSILON.number_words -= 1

  ////

  def apply() = new PreciseNumber(true, precision_digits) with RealNumber
  def apply(b: Boolean, precision: Int) = new PreciseNumber(b, precision) with RealNumber
  def apply(size: Int, b: Boolean) = new PreciseNumber(size, b) with RealNumber
  def apply(in: RealNumber) = new PreciseNumber(in.asInstanceOf[PreciseNumber]) with RealNumber
  def apply(d: Double) = new PreciseNumber(d, precision_digits) with RealNumber
  def apply(d: Double, precision: Int) = new PreciseNumber(d, precision) with RealNumber
  def apply(str: String) = new PreciseNumber(str, precision_digits) with RealNumber
  def apply(str: String, precision: Int) = new PreciseNumber(str, precision) with RealNumber
  def apply(in: IntegerNumber) = new PreciseNumber(in.asInstanceOf[PreciseNumber]) with RealNumber
  def apply(size: Int) = new PreciseNumber(size, false) with RealNumber
  def apply(mpc: ComplexNumber) = new PreciseNumber(mpc, precision_digits) with RealNumber
  def apply(mpc: ComplexNumber, precision: Int) = {
    val x: PreciseNumber = new PreciseNumber(true, precision) with RealNumber
    eq(mpc.r, this, Math.min(nw, maxnw - 1))
    x
  }
  
}

