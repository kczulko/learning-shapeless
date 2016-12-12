package com.github.kczulko.chapter2.adts

sealed trait Tree[A]
case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
case class Leaf[A](a: A) extends Tree[A]
