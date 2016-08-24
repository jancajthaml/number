package com.github.jancajthaml.number

private[number] object Shared {

  val AL2 = 0.301029995663981195

  val CL2 = 1.4426950408889633

  val CPI = 3.141592653589793

  val ALT = 0.693147180559945309

  val LOGE10 = 2.302585092994046

  val NIT = 3

  val N30 = 1073741824

  private var precision_current: Int = 0

  var pointer: Int = 0

  var round: Int = 0

  var precision_digits_current: Int = 0

  var precision_words: Int = 0

  var precision_digits: Int = 0

  var precision_digits_out: Int = 0

  var ellog10: Int = 0

  var mp2: Int = 0

  var mp21: Int = 0

  var nw: Int = 0

  var uu1: Array[Complex] = null

  var uu2: Array[Complex] = null

  setMaximumPrecision(300)

  def setMaximumPrecision(p: Int) {
    if (p == precision_current) return
    p = Math.min(Math.abs(p), 3600)
    precision_current = p
    pointer = 7
    round = 1
    precision_digits = precision_current
    precision_digits_out = 10000000
    ellog10 = 10 - precision_digits
    precision_words = ((precision_digits / 7.224719896) + 1).toInt
    mp2 = precision_words + 2
    mp21 = mp2 + 1
    precision_digits_current = precision_digits_out
    nw = precision_words
    val n = nw + 1
    var ti = 0.0
    var j = 0
    var i = 0
    var t1 = 0.75 * n
    val m = (CL2 * Math.log(t1) + 1.0 - 5.6843418860808015e-14).toInt
    val mq = m + 2
    val nq = (Math.pow(2, mq)).toInt
    uu1 = Array.ofDim[Complex](nq)
    uu1(0) = new Complex(mq, 0)
    var ku = 1
    var ln = 1
    j = 1
    while (j <= mq) {
      t1 = Math.PI / ln
      i = 0
      while (i <= ln - 1) {
        ti = i * t1
        uu1(i + ku) = new Complex(Math.cos(ti), Math.sin(ti))
        i += 1
      }
      ku += ln
      ln <<= 1
      j += 1
    }
    var tpn = 0.0
    var k = 0
    t1 = 0.75 * n
    uu2 = Array.ofDim[Complex](mq + nq)
    ku = mq + 1
    uu2(0) = new Complex(mq, 0)
    var mm = 0
    var nn = 0
    var mm1 = 0
    var mm2 = 0
    var nn1 = 0
    var nn2 = 0
    var iu = 0
    k = 2
    while (k <= mq - 1) {
      uu2(k - 1) = new Complex(ku, 0)
      mm = k
      nn = (Math.pow(2, mm)).toInt
      mm1 = (mm + 1) / 2
      mm2 = mm - mm1
      nn1 = (Math.pow(2, mm1)).toInt
      nn2 = (Math.pow(2, mm2)).toInt
      tpn = 2.0 * Math.PI / nn
      j = 0
      while (j < nn2) {
        i = 0
        while (i < nn1) {
          iu = ku + i + j * nn1
          t1 = tpn * i * j
          uu2(iu - 1) = new Complex(Math.cos(t1), Math.sin(t1))
          i += 1
        }
        j += 1
      }
      ku += nn
      k += 1
    }
  }

  def nint(x: Double): Double = {
    if ((x < 0)) Math.ceil(x - .5) else Math.floor(x + .5)
  }

  def fSign(a: Double, b: Double): Double = {
    (if (b >= 0) Math.abs(a) else -Math.abs(a))
  }

  def log10(i: Double): Double = (Math.log(i) / LOGE10)

  def precisionToSize(precision: Int): Int = {
    val maxnw = ((precision / 7.224719896) + 4).toInt
    if ((maxnw < 8)) 8 else maxnw
  }

}
