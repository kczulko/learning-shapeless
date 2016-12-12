package com.github.kczulko

import shapeless.{HList, ::, HNil}
import shapeless.syntax.singleton._
import scala.reflect.runtime.universe._

object Main {
  def main(args: Array[String]): Unit = {
    val product: String :: HNil = "Sunday" :: HNil
    val list = 1 :: true :: Nil

    var x  = 42.narrow
    val another: Int = 42
    println(reify(x))
    println(reify(another))
  }
}
