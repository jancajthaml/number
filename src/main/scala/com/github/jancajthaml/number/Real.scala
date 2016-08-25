package com.github.jancajthaml.number

object Real {

  private lazy val cache = new Array[Real](1025)

  def apply(i: Int): Real =
    if (i > -512 && i <= 512) {
      val offset = i + 512
      var n = cache(offset)
      if (n eq null) {
        n = new Real(new RealNumber(i))
        cache(offset) = n
      }
      n
    } else new Real(new RealNumber(i))

  def apply(l: Long): Real =
    if (l > -512 && l <= 512) apply(l.toInt)
    else new Real(new RealNumber(l))

  def apply(d: Double): Real = new Real(new RealNumber(d))

  def apply(x: Array[Char]): Real =  new Real(new RealNumber(x.toString))

  def apply(x: String): Real =  new Real(new RealNumber(x))

  implicit def int2bigDecimal(i: Int): Real = apply(i)

  implicit def long2bigDecimal(l: Long): Real = apply(l)

  implicit def double2bigDecimal(d: Double): Real = apply(d)

  implicit def bigDecimal2ordered(x: Real): Ordered[Real] =
    new Ordered[Real] with Proxy {
      def self: Any = x
      def compare(y: Real): Int = x.value.equals(y.value)
  }
}

@serializable
class Real(val value: BigDec) extends java.lang.Number {

  override def hashCode(): Int = this.value.hashCode()

  override def equals(that: Any): Boolean = that match {
    case that: Real => this equals that
    case that: java.lang.Double => this.value.doubleValue == that.doubleValue
    case that: java.lang.Float  => this.value.floatValue == that.floatValue
    case that: java.lang.Number => this equals Real(that.longValue)
    case that: java.lang.Character => this equals Real(that.charValue.asInstanceOf[Int])
    case _ => false
  }

  def equals (that: Real): Boolean = this.value.equals(that.value)

  def compare (that: Real): Int = this.value.compareTo(that.value)

  def <= (that: Real): Boolean = this.value.compareTo(that.value) <= 0

  def >= (that: Real): Boolean = this.value.compareTo(that.value) >= 0

  def <  (that: Real): Boolean = this.value.compareTo(that.value) <  0

  def >  (that: Real): Boolean = this.value.compareTo(that.value) > 0

  def +  (that: Real): Real = new Real(this.value.add(that.value))

  def -  (that: Real): Real = new Real(this.value.subtract(that.value))

  def *  (that: Real): Real = new Real(this.value.multiply(that.value))

  def /  (that: Real): Real = new Real(this.value.divide(that.value))

  def min (that: Real): Real = new Real(this.value.min(that.value))

  def max (that: Real): Real = new Real(this.value.max(that.value))

  def unary_- : Real = new Real(this.value.negate())

  def abs: Real = new Real(this.value.abs())

  def signum: Int = this.value.signum()

  override def byteValue = intValue.toByte

  override def shortValue = intValue.toShort

  def charValue = intValue.toChar

  def intValue = this.value.intValue

  def longValue = this.value.longValue
  
  def floatValue = this.value.floatValue

  def doubleValue = this.value.doubleValue

  override def toString(): String = this.value.toString()

}