package com.github.jancajthaml.number;

class RealNumber extends RealNumberStub {

  def +(that: RealNumberStub): RealNumber = this.add(that).asInstanceOf[RealNumber]
  def -(that: RealNumberStub): RealNumber = this.subtract(that).asInstanceOf[RealNumber]
  def *(that: RealNumberStub): RealNumber = this.multiply(that).asInstanceOf[RealNumber]
  def /(that: RealNumberStub): RealNumber = this.divide(that).asInstanceOf[RealNumber]
  def **(that: Int): RealNumber = this.pow(that).asInstanceOf[RealNumber]

}