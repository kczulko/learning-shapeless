package com.github.kczulko

import shapeless.{HList, ::, HNil}


object Main {
  def main(args: Array[String]): Unit = {
    val product: String :: HNil = "Sunday" :: HNil
//    println { product }
    val list = 1 :: true :: Nil
  }
}
