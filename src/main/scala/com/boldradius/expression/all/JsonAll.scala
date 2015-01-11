package com.boldradius.expression.all


import com.boldradius.expression._
import com.boldradius.expression.some.{Minus, Plus, Number, Expression}

trait JsonAll[A] {
  def convertToJson(value:A):JsonValue
}

object JsonAll{

  implicit val expressionJson = new JsonAll[Expression] {
    def convertToJson(expr: Expression):JsonValue = expr match {
      case Number(value) => JsonNumber(value)
      case Plus(left,right) => JsonObject(
          Map("op" -> JsonString("+"),"left" -> convertToJson(left),"right" -> convertToJson(right)))
      case Minus(left,right) => JsonObject(
        Map("op" -> JsonString("-"),"left" -> convertToJson(left),"right" -> convertToJson(right)))
    }
  }


  implicit val intJson = new JsonAll[Int]{
    def convertToJson(value:Int):JsonValue = JsonNumber(value)
  }

  implicit def tupleJson[T1:JsonAll, T2:JsonAll] = new JsonAll[(T1,T2)]{

    def convertToJson(pair:(T1,T2)):JsonValue =
      JsonObject(
        Map("first" -> implicitly[JsonAll[T1]].convertToJson(pair._1),
        "second" -> implicitly[JsonAll[T2]].convertToJson(pair._2))
      )
  }



}
