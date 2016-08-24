package com.github.jancajthaml.number

import Complex._

private[number] object Complex {

  private def hypot(x: Double, y: Double): Double = Math.sqrt(x * x + y * y)

}

private[number] class Complex(private var re: Double, private var im: Double) extends Number with Cloneable {

  private def rtop(): Complex = {
    new Complex(hypot(re, im), Math.atan2(im, re))
  }

  private def ptor(): Complex = {
    new Complex(re * Math.cos(im), re * Math.sin(im))
  }

  def this(r: Double) {
    this(r, 0)
  }

  def this() {
    this(0, 0)
  }

  def this(d: Complex) {
    this()
    re = d.re
    im = d.im
  }

  def set(re: Double, im: Double) {
    this.re = re
    this.im = im
  }

  def real(): Double = re

  def aimag(): Double = im

  def abs(): Double = hypot(re, im)

  def arg(): Double = Math.atan2(im, re)

  def conjg(): Complex = new Complex(re, -im)

  def exp(d: Complex): Complex = {
    val r = Math.exp(re)
    new Complex(r * Math.cos(im), r * Math.sin(im))
  }

  def pow(r: Double): Complex = {
    val polar = this.rtop()
    polar.re = Math.pow(polar.re, r)
    polar.im *= r
    polar.ptor()
  }

  def power(r: Int): Complex = r match {
    case 0 => new Complex(1)
    case 1 => new Complex(this)
    case 2 => this.multiply(this)
    case _ => 
      var polar = this.rtop()
      polar.re = Math.pow(polar.re, r)
      polar.im *= r
      polar.ptor()

  }

  def sqrt(): Complex = {
    val polar = this.rtop()
    polar.re = Math.sqrt(polar.re)
    polar.im = 0.5 * polar.im
    polar.ptor()
  }

  def add(d2: Complex): Complex = new Complex(re + d2.re, im + d2.im)

  def negate(): Complex = new Complex(-re, -im)

  def subtract(d2: Complex): Complex = new Complex(re - d2.re, im - d2.im)

  def multiply(d2: Complex): Complex = {
    new Complex(re * d2.re - im * d2.im, re * d2.im + im * d2.re)
  }

  def divide(d2: Complex): Complex = {
    val denom = d2.re * d2.re + d2.im * d2.im
    new Complex((re * d2.re + im * d2.im) / denom, (im * d2.re - re * d2.im) / denom)
  }

  def addEqual(d: Complex): Complex = {
    re += d.re
    im += d.im
    this
  }

  def subtractEqual(d: Complex): Complex = {
    re -= d.re
    im -= d.im
    this
  }

  def multiplyEqual(d: Complex): Complex = {
    val oldRe = re
    val oldDRe = d.re
    re = re * d.re - im * d.im
    im = im * oldDRe + oldRe * d.im
    this
  }

  def divideEqual(d: Complex): Complex = {
    val denom = d.re * d.re + d.im * d.im
    val oldRe = re
    val oldDRe = d.re
    re = (re * d.re + im * d.im) / denom
    im = (im * oldDRe - oldRe * d.im) / denom
    this
  }

  override def equals(d1: Any): Boolean = {
    try {
      (re == d1.asInstanceOf[Complex].re && im == d1.asInstanceOf[Complex].im)
    } catch {
      case e: ClassCastException => false
    }
  }

  override def clone(): AnyRef = new Complex(re, im)

  override def toString(): String = s"(${new java.lang.Double(re)},${new java.lang.Double(im)})"

  override def hashCode(): Int = (new java.lang.Double(re / 2 + im / 2)).hashCode

  override def byteValue(): Byte = re.toByte

  def doubleValue(): Double = re

  def floatValue(): Float = re.toFloat

  def intValue(): Int = re.toInt

  def longValue(): Long = re.toLong

  override def shortValue(): Short = re.toShort

}
