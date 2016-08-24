package com.github.jancajthaml.number

import Chunk._

private[number] object Chunk {

  def dpdec(a: Chunk, b: Chunk) {
    if (a.a != 0.0) {
      val t1 = 0.3010299956639812 * a.n + log10(Math.abs(a.a))
      b.n = t1.toInt
      if (t1 < 0.0) b.n -= 1
      b.a = fSign(Math.pow(10.0, (t1 - b.n)), a.a)
    } else {
      b.a = 0.0
      b.n = 0
    }
  }

}

private[number] class Chunk() extends Shared {

  var a: Double = 0.0

  var n: Int = 0

  def this(A: Double) {
    this(A, 0)
  }

  def this(A: Double, N: Int) {
    this()
    a = A
    n = N
  }

  def value(): Double = a * Math.pow(2, n)

}
