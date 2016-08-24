package com.github.jancajthaml.number

class IntegerNumber extends PreciseNumber(true, precision_digits) {

  def this(precision: Int) {
    super(true, precision)
  }

  def this(in: IntegerNumber) {
    super(in.asInstanceOf[PreciseNumber])
  }

  def this(d: Double, precision: Int) {
    super(true, precision)
    val mpt1 = new PreciseNumber(6, false)
    val mpt2 = new PreciseNumber(8, false)
    dmc(new Chunk(d), mpt1)
    infr(mpt1, this, mpt2, Math.min(maxnw - 2, nw))
  }

  def this(str: String) {
    this(str, precision_digits)
  }

  def this(str: String, precision: Int) {
    super(true, precision)
    val lnw = Math.min(nw, maxnw - 2)
    val nw2 = lnw + 2
    val mpt1 = new PreciseNumber(nw2, false)
    val mpt2 = new PreciseNumber(nw2, false)
    dexc(str.toCharArray(), str.length, mpt1, lnw)
    infr(mpt1, this, mpt2, lnw)
  }

  def this(mpr: RealNumber) {
    this(mpr, precision_digits)
  }

  def this(mpr: RealNumber, precision: Int) {
    super(true, precision)
    val lnw = Math.min(nw, maxnw - 2)
    val mpt1 = new PreciseNumber(lnw + 1, false)
    val mpt2 = new PreciseNumber(lnw + 2, false)
    eq(mpr, mpt1, lnw)
    infr(mpt1, this, mpt2, lnw)
  }

  def this(mpc: ComplexNumber) {
    this(mpc, precision_digits)
  }

  def this(mpc: ComplexNumber, precision: Int) {
    super(true, precision)
    val lnw = Math.min(nw, maxnw - 2)
    val mpt1 = new PreciseNumber(lnw + 1, false)
    val mpt2 = new PreciseNumber(lnw + 2, false)
    eq(mpc.r, mpt1, lnw)
    infr(mpt1, this, mpt2, lnw)
  }

  def assign(ja: PreciseNumber): IntegerNumber = {
    if (ja != this) {
      if (ja.maxnw == this.maxnw && ja.isInstanceOf[IntegerNumber]) PreciseNumber.eq(ja, this, Math.min(nw, 
        this.maxnw - 1)) else {
        val nw1 = Math.min(nw, ja.maxnw - 1)
        val nw2 = Math.min(nw, maxnw - 2)
        val mpt1 = new RealNumber()
        val mpt2 = new RealNumber()
        eq(ja, mpt1, nw1)
        infr(mpt1, this, mpt2, nw2)
      }
    }
    this
  }

  def add(ja: IntegerNumber): IntegerNumber = {
    val res = new IntegerNumber()
    val mpt1 = new IntegerNumber()
    val mpt2 = new IntegerNumber()
    add(ja, this, mpt1, nw)
    infr(mpt1, res, mpt2, nw)
    res
  }

  def subtract(ja: IntegerNumber): IntegerNumber = {
    val res = new IntegerNumber()
    val mpt1 = new IntegerNumber()
    val mpt2 = new IntegerNumber()
    sub(this, ja, mpt1, nw)
    infr(mpt1, res, mpt2, nw)
    res
  }

  def negate(): IntegerNumber = {
    val res = new IntegerNumber()
    eq(this, res, nw)
    res.sign = !this.sign
    res
  }

  def multiply(ja: IntegerNumber): IntegerNumber = {
    val res = new IntegerNumber()
    val mpt1 = new IntegerNumber()
    val mpt2 = new IntegerNumber()
    _mul(ja, this, mpt1, nw)
    infr(mpt1, res, mpt2, nw)
    res
  }

  def divide(ja: IntegerNumber): IntegerNumber = {
    val res = new IntegerNumber()
    val mpt1 = new IntegerNumber()
    val mpt2 = new IntegerNumber()
    _div(this, ja, mpt1, nw)
    infr(mpt1, res, mpt2, nw)
    res
  }

  def mod(ja: IntegerNumber): IntegerNumber = {
    val res = new IntegerNumber()
    val mpt1 = new IntegerNumber()
    val mpt2 = new IntegerNumber()
    val mpt3 = new IntegerNumber()
    _div(this, ja, mpt1, nw)
    infr(mpt1, mpt2, mpt3, nw)
    _mul(ja, mpt2, mpt1, nw)
    sub(this, mpt1, res, nw)
    res
  }

  def abs(): IntegerNumber = {
    val res = new IntegerNumber()
    eq(this, res, nw)
    res.sign = true
    res
  }

  def max(i: IntegerNumber): IntegerNumber = {
    if ((compare(this, i, nw) >= 0)) this else i
  }

  def min(i: IntegerNumber): IntegerNumber = {
    if ((compare(this, i, nw) < 0)) this else i
  }

  def sign(i: PreciseNumber): IntegerNumber = {
    val res = new IntegerNumber()
    eq(this, res, nw)
    res.sign = i.sign
    res
  }

  def pow(exponent: IntegerNumber): IntegerNumber = {
    val res = new IntegerNumber()
    val mpt1 = new IntegerNumber()
    val mpt2 = new IntegerNumber()
    _log(this, RealNumber.PI, RealNumber.LOG2, mpt1, nw)
    _mul(mpt1, exponent, mpt2, nw)
    _exp(mpt2, RealNumber.PI, RealNumber.LOG2, mpt1, nw)
    nint(mpt1, res, nw)
    res
  }

  def pow(exponent: Int): IntegerNumber = {
    val res = new IntegerNumber()
    val mpt1 = new IntegerNumber()
    _npw(this, exponent, mpt1, nw)
    nint(mpt1, res, nw)
    res
  }

}