package com.boldradius.jsonexample.expression.all


import com.boldradius.jsonexample.expression._

trait JsonAll[A] {
  def convertToJson(value:A):JsonValue
}

object JsonAll{

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
