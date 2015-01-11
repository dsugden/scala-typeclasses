package com.boldradius.expression.some

import com.boldradius.expression._

trait Json[A] {
  def convertToJson(value:A):JsonValue
}

object Json{

  implicit val expressionJson = new Json[Expression] {
    def convertToJson(expr: Expression):JsonValue = expr match {
      case Number(value) => JsonNumber(value)
      case Plus(left,right) => JsonObject(
          Map("op" -> JsonString("+"),"left" -> convertToJson(left),"right" -> convertToJson(right)))
      case Minus(left,right) => JsonObject(
        Map("op" -> JsonString("-"),"left" -> convertToJson(left),"right" -> convertToJson(right)))
    }
  }

}
