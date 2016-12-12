import shapeless.ops.hlist.{IsHCons, Last}
import shapeless.{::, Generic, HList, HNil}

def getRepr[A](value: A)(implicit gen: Generic[A]): gen.Repr =
  gen.to(value)

case class Vec(x: Int, y: Int)
case class Rect(a: Vec, b: Vec)

getRepr(Vec(1,2))
getRepr(Rect(Vec(1,2), Vec(1,2)))

val last1 = Last[String :: Int :: HNil]
val last2 = Last[Int :: String :: HNil]

last1("foo" :: 123 :: HNil)
last2(123 :: "bar" :: HNil)

trait Second[L <: HList] {
  type Out
  def apply(v: L): Out
}

object Second {
  type Aux[L <: HList, O] = Second[L] { type Out = O }
  def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] = inst

  implicit def hlistSecond[A, B, Others <: HList]: Aux[A :: B :: Others, B] =
    new Second[A :: B :: Others] {
      override type Out = B
      override def apply(v: A :: B :: Others): B = v.tail.head
    }
}

val sec = Second[Int :: String :: Boolean :: HNil]
sec(123 :: "dupa" :: false :: HNil)

def lastField[A, R <: HList](input: A)(
  implicit
  gen: Generic[A] { type Repr = R },
  last: Last[R]
): last.Out = last.apply(gen.to(input))

lastField(Rect(Vec(1,1), Vec(2,2)))

case class Wrapper(v: Int)

def getWrappedValue[A, R <: HList, Head, Tail <: HList](v: A)
                         (implicit gen: Generic[A] { type Repr = R },
                          isHCons: IsHCons.Aux[R, Head, Tail]
                         ): Head = gen.to(v).head

getWrappedValue(Wrapper(5))
