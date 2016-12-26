import shapeless._
import shapeless.ops.hlist

object sizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int] = at(identity)
  implicit val stringCase: Case.Aux[String, Int] = at(_.length)
  implicit val booleanCase: Case.Aux[Boolean, Int] = at(b => if (b) 1 else 0)
}

(0 :: "hello" :: true :: HNil).map(sizeOf)

object valueAndSizeOf extends Poly1 {
  implicit val intCase: Case.Aux[Int, Int :: Int :: HNil] =
    at(num => num :: num :: HNil)

  implicit val stringCase: Case.Aux[String, String :: Int :: HNil] =
    at(s => s :: s.length :: HNil)

  implicit val booleanCase: Case.Aux[Boolean, Boolean :: Int :: HNil] =
    at(bool => bool :: (if (bool) 1 else 0) :: HNil)
}

(10 :: "hello" :: true :: HNil).flatMap(valueAndSizeOf)

trait ProductMapper[A, B, P] {
  def apply(a: A): B
}

implicit def genericProductMapper[
  A, B,
  P <: Poly,
  ARepr <: HList,
  BRepr <: HList
](
 implicit
 aGen : Generic.Aux[A, ARepr],
 bGen : Generic.Aux[B, BRepr],
 mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
): ProductMapper[A, B, P] =
  (a : A) => bGen.from(mapper.apply(aGen.to(a)))

implicit class ProductMapperOps[A](a: A) {
  class Builder[B] {
    def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A,B,P]): B =
      pm.apply(a)
  }
  def mapTo[B]: Builder[B] = new Builder[B]
}

object conversions extends Poly1 {
  implicit val intCase: Case.Aux[Int, Boolean] = at(_ > 0)
  implicit val boolCase: Case.Aux[Boolean, Int] = at(if(_) 1 else 0)
  implicit val stringCase: Case.Aux[String, String] = at(identity)
}

case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)

IceCream1("Sundae", 1, false).mapTo[IceCream2](conversions)
