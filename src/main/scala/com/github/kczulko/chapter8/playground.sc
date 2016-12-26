import java.util

import com.github.kczulko.chapter2.adts.IceCream
import shapeless._
import shapeless.ops.nat.ToInt
import shapeless.ops.{coproduct, hlist, nat}

val hlistLength = hlist.Length[String :: Int :: Boolean :: HNil]

val comproductLenght = coproduct.Length[Double :+: Char :+: CNil]

Nat.toInt[hlistLength.Out]

trait SizeOf[A] {
  def value: Int
}

def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

implicit def genericSizeOf[A, L <: HList, N <: Nat]
(
  implicit
  gen: Generic.Aux[A, L],
  size: hlist.Length.Aux[L, N],
  sizeToInt: nat.ToInt[N]
): SizeOf[A] = new SizeOf[A] {
  override def value: Int = sizeToInt.apply()
}

sizeOf[IceCream]

import org.scalacheck._

for (i <- 1 to 3) println(Arbitrary.arbitrary[Int].sample)
for (i <- 1 to 3) println(Arbitrary.arbitrary[(Boolean, Byte)].sample)

trait Random[A] {
  def get: A
}

def random[A](implicit r: Random[A]): A = r.get

def createRandom[A](func: () => A): Random[A] =
  new Random[A] {
    def get: A = func()
  }

implicit val intRandom: Random[Int] = createRandom(() => scala.util.Random.nextInt(10))
implicit val charRandom: Random[Char] = createRandom(() => ('A'.toInt + scala.util.Random.nextInt(26)).toChar)
implicit val booleanRandom: Random[Boolean] = createRandom(() => scala.util.Random.nextBoolean)

implicit def genericRandom[A, R]
(
  implicit
  gen: Generic.Aux[A, R],
  random: Lazy[Random[R]]
): Random[A] =
  createRandom(() => gen.from(random.value.get))

implicit val hnilRandom: Random[HNil] = createRandom(() => HNil)

implicit def hlistRandom[H, T <: HList]
(
  implicit
  hRandom: Lazy[Random[H]],
  tRandom: Random[T]
): Random[H :: T] =
  createRandom(() => hRandom.value.get :: tRandom.get)

case class Cell(col: Char, row: Int)

for (i <- 1 to 5) println(random[Cell])

//################################################
// RANDOM COPRODUCTS
//################################################

implicit val cnilRandom: Random[CNil] =
  createRandom(() => throw new Exception("Inconceivable!"))

implicit def coproductRandom[H, T <: Coproduct, L <: Nat]
(
  implicit
  hRandom: Lazy[Random[H]],
  tRandom: Random[T],
  tLenght: coproduct.Length.Aux[T, L],
  tLenghtAsInt: ToInt[L]
): Random[H :+: T] =
 createRandom(() => {
   val length = 1 + tLenghtAsInt.apply()
   val chooseH = scala.util.Random.nextDouble() < (1.0 / length)
   if (chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
 })

sealed trait Light
case object Red extends Light
case object Amber extends Light
case object Green extends Light


for (i <- 1 to 20) println(random[Light])