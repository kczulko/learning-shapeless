import com.github.kczulko.chapter2.adts.IceCream
import shapeless._
import shapeless.ops.hlist
import shapeless.ops.hlist.{Align, Intersection}
import shapeless.ops.record.Merger

import scala.Symbol

trait Penultimate[L] {
  type Out
  def apply(l: L): Out
}

object Penultimate {
  type Aux[L, O] = Penultimate[L] { type Out = O }
  def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p
}

implicit def hlistPenultimate[L <: HList, M <: HList, O](
  implicit
  init: hlist.Init.Aux[L, M],
  last: hlist.Last.Aux[M, O]
): Penultimate.Aux[L,O] =
  new Penultimate[L] {
    type Out = O
    override def apply(l: L): O = last.apply(init.apply(l))
  }

type BigList = String :: Int :: Boolean :: Double :: HNil
val bigLst: BigList = "foo" :: 2 :: true :: 3.2 :: HNil

Penultimate[BigList].apply(bigLst)

implicit class PenultimateOps[A](a: A) {
  def penultimate(implicit inst: Penultimate[A]): inst.Out = inst.apply(a)
}

("foo" :: 2 :: true :: 3.2 :: HNil).penultimate

implicit def genericPenultimate[A, R, O](
  implicit
  generic: Generic.Aux[A,R],
  penultimate: Penultimate.Aux[R, O]
): Penultimate.Aux[A,O] = {
  new Penultimate[A] {
    override type Out = O
    override def apply(l: A): O = penultimate.apply(generic.to(l))
  }
}

IceCream("",2,false).penultimate

case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)
case class IceCreamV1a(name: String, numCherries: Int)
case class IceCreamV2a(name: String, inCone: Boolean)
case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
case class IceCreamV2c(
    name: String, inCone: Boolean, numCherries: Int, numWaffles: Int
)

trait Migration[A, B] {
  def apply(a: A): B
}

implicit class MigrationOps[A](a: A) {
  def migrateToB[B](implicit migration: Migration[A,B]): B = migration.apply(a)
}

import cats.Monoid
import cats.instances.all._
import shapeless.labelled.{ field, FieldType }

def createMonoid[A](zero: A)(op: (A,A) => A): Monoid[A] = new Monoid[A] {
  override def empty: A = zero
  override def combine(x: A, y: A): A = op(x,y)
}

implicit val hNilMonoid = createMonoid[HNil](HNil)((_,_) => HNil)

implicit def emptyHlist[K <: Symbol, H, T <: HList]
(
  implicit
  hMonoid: Lazy[Monoid[H]],
  tMonoid: Monoid[T]
): Monoid[FieldType[K, H] :: T] =
  createMonoid(
    field[K](hMonoid.value.empty) :: tMonoid.empty
  )( (a,b) =>
    field[K](hMonoid.value.combine(a.head, b.head)) ::
      tMonoid.combine(a.tail, b.tail)
  )

implicit def genericMigration[
A, B, ARepr <: HList, BRepr <: HList,
Common <: HList, Added <: HList, Unaligned <: HList
](
   implicit
   aGen : LabelledGeneric.Aux[A, ARepr],
   bGen : LabelledGeneric.Aux[B, BRepr],
   inter : hlist.Intersection.Aux[ARepr, BRepr, Common],
   diff : hlist.Diff.Aux[BRepr, Common, Added],
   monoid : Monoid[Added],
   prepend : hlist.Prepend.Aux[Common, Added, Unaligned],
   align : hlist.Align[Unaligned, BRepr]
 ): Migration[A, B] = (a: A) =>
  bGen.from(
    align(
      prepend(
        inter(aGen.to(a)),
        monoid.empty
      )
    )
  )

IceCreamV1("", 3, inCone = true).migrateToB[IceCreamV2b]
IceCreamV1("", 3, inCone = true).migrateToB[IceCreamV2c]