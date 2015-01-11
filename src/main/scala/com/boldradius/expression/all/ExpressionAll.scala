package com.boldradius.expression.all

trait ExpressionAll[A] {
  def value(expr:A):Int
}

object ExpressionAll{

  implicit val intExpression = new ExpressionAll[Int]{
    def value(n:Int):Int = n
  }

  implicit def pairPlusExpression[T1:ExpressionAll, T2:ExpressionAll] = new ExpressionAll[(T1,T2)] {
    def value(pair: (T1, T2)): Int =
      implicitly[ExpressionAll[T1]].value(pair._1) + implicitly[ExpressionAll[T2]].value(pair._2)
  }

}


object ExpressionAllEvaluator{
  def evaluate[A: ExpressionAll](expr:A):Int = implicitly[ExpressionAll[A]].value(expr)
}