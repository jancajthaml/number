package com.github.jancajthaml.number;

class IntegerNumber extends IntegerNumberStub {


  def +(that: IntegerNumberStub): IntegerNumber = this.add(that).asInstanceOf[IntegerNumber]
  def -(that: IntegerNumberStub): IntegerNumber = this.subtract(that).asInstanceOf[IntegerNumber]
  def *(that: IntegerNumberStub): IntegerNumber = this.multiply(that).asInstanceOf[IntegerNumber]
  def /(that: IntegerNumberStub): IntegerNumber = this.divide(that).asInstanceOf[IntegerNumber]
  def **(that: Int): IntegerNumber = this.pow(that).asInstanceOf[IntegerNumber]


}