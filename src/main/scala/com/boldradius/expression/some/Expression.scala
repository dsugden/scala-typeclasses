package com.boldradius.expression.some

/**
 * from Typeclasses with Dan Rosen  https://www.youtube.com/watch?v=sVMES4RZF-8
 */

sealed trait Expression
case class Number(value:Int) extends Expression
case class Plus(left:Expression, right:Expression) extends Expression
case class Minus(left:Expression, right:Expression) extends Expression

object ExpressionEvaluator{
  def value(e:Expression  ):Int  = e match {
    case Number(v) => v
    case Plus(left,right) => value(left) + value(right)
    case Minus(left,right) => value(left) - value(right)
  }
}


