package com.boldradius.expression.some

import com.boldradius.expression._

object JsonWriter {

  def write(value: JsonValue):String = value match {
    case JsonObject(entries) => {
      val serializedEntries = for((key,value) <- entries) yield key + ":" + write(value)
      s"{ ${serializedEntries.mkString(",")} }"
    }

    case JsonArray(entries) => {
      val serializedEntries = entries.map(write)
      s"[ ${serializedEntries.mkString(",")}} ]"
    }

    case JsonString(value) => s""""$value""""
    case JsonNumber(value) => value.toString
    case JsonBoolean(value)=> value.toString
  }



  /**

  we want to be able to write anything to json

  def write(value:???):String = write(value.???)

  */


  /**
   * Using subtype polymorphism
   *
   *
   *   def write(value: JsonConvertible):String = write(value.convertToJson)
   *
   *
   * Problem: now Expression must extend JsonConvertible... what if we can't modify Expression  (its a lib)
   *
   *
   *                     JsonWriter
   *                      /     \
   *             JsonValue <---  JsonConvertible
   *                              \
   *   ExpressionEvaluator ---> Expression
   *
   *
   * subtype is unsatisfactory, we need ad-hoc polymorphism
   *
   */


  /**
   *
   * Using Context Bounds , we say A is a member of the Json typeclass
   */
  def write[A:Json](value:A):String  = write( implicitly[Json[A]].convertToJson(value) )




















  def main(args:Array[String]):Unit = {

    import com.boldradius.expression.some.Json._

    val expr = Plus(Number(3),Minus(Number(2), Number(1)))

    println( JsonWriter.write[Expression](expr) )
  }






}
