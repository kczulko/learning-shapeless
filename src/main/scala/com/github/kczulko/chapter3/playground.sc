import com.github.kczulko.chapter2.adts._
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy, the}

trait CsvEncoder[A] {
  def encode(a: A): List[String]
}

// old type class instance for Employee to csv conversion
//implicit object EmployeeCsvEncoder extends CsvEncoder[Employee] {
//  override def encode(a: Employee): List[String] = List(
//    a.name,
//    a.id.toString,
//    a.isManager.toString
//  )
//}

// old type class instance for IceCream to csv conversion
//implicit object IceCreamCsvEncoder extends CsvEncoder[IceCream] {
//  override def encode(a: IceCream): List[String] = List(
//    a.name,
//    a.numCherries.toString,
//    a.inCone.toString
//  )
//}

// companion object / materializer
object CsvEncoderMaterializer {
  def apply[A](implicit enc: A): A = enc

  def instance[A](f: A => List[String]) = new CsvEncoder[A] {
    override def encode(a: A): List[String] = {
      f(a)
    }
  }
}

object CsvEncoders {
  implicit def pairEncoder[A : CsvEncoder, B : CsvEncoder]: CsvEncoder[(A,B)] =
//    a => implicitly[CsvEncoder[A]].encode(a._1) ++ implicitly[CsvEncoder[B]].encode(a._2)
    // using custom summoner and implicitly
    a => CsvEncoderMaterializer[CsvEncoder[A]].encode(a._1) ++ implicitly[CsvEncoder[B]].encode(a._2)
}

def writeCsv[A : CsvEncoder](values: List[A]): String = {
  // using shapeless' the
  values.map(value => the[CsvEncoder[A]].encode(value).mkString(",")).mkString("\n")
}

val employees = List(
  Employee("fdsa", 1234, true),
  Employee("fdsa", 1234, false)
)

val iceCreams = List(
  IceCream("vanilla", 123, false),
  IceCream("vanilla", 123, true)
)

case class Kczulko(bored: Boolean = true)

import CsvEncoders._

//==============================================
// deriving encoder from hlist
//==============================================

def createEncoder[A](f: A => List[String]): CsvEncoder[A] =
  a => f(a)

implicit val stringEncoder = createEncoder[String](List(_))
implicit val intEncoder = createEncoder[Int](i => List(i.toString))
implicit val booleanEncoder = createEncoder[Boolean](b => List(if (b) "yes" else "no"))
implicit val hnilEncoder = createEncoder[HNil](_ => Nil)

implicit def hlistEncoder[H, T <: HList](
  implicit hEncoder: Lazy[CsvEncoder[H]],
  tEncoder: CsvEncoder[T]
): CsvEncoder[H :: T] = createEncoder {
  case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
}

val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
reprEncoder.encode("Dupa" :: 2 :: true :: HNil)

// old IceCream encoder - not parameterized yet
//
//implicit val newIceCreamEncoder: CsvEncoder[IceCream] = {
//  // gen is a dual representation of IceCream type
//  val gen = Generic[IceCream]
//  // enc is a dual encoder based on gen.Repr which is a type of CsvEncoder[String::Int::Boolean::HNil]
//  val enc = the[CsvEncoder[gen.Repr]]
//  createEncoder { iceCream =>
//    // dual encoder encodes transformed instance of 'normal' iceCream by using gen val
//    enc.encode(gen.to(iceCream))
//  }
//}

implicit def genericEncoder[A, B](implicit gen: Generic[A] { type Repr = B }, enc: Lazy[CsvEncoder[B]]) = {
  createEncoder { item: A => enc.value.encode(gen.to(item)) }
}

println { writeCsv(iceCreams) }
println { writeCsv(employees zip iceCreams) }


case class Foo(bar: String, num: Int)
val v = Generic[Foo]
writeCsv(List(new Foo("bar", 2)))

import java.util.Date
case class Bar(baz: String, date: Date, foo: Foo)
implicit val dateEncoder = createEncoder[Date](d => List(d.toGMTString))

// HOW TO AVOID THIS IN CASE OF Foo hllistEncoding??? WITHOUT THIS LINE NEXT PRINT WON'T WORK :(
// NOW IT'S CLEAR! USE Lazy[T]
//implicit val fooEncoder = hlistEncoder(stringEncoder, hlistEncoder(intEncoder, hnilEncoder))
writeCsv(List(new Bar("foo", new Date(), Foo("bar", 1))))

//===============================================
// coproduct encoders:
//===============================================

implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                 hEnc: Lazy[CsvEncoder[H]],
                                                 tEnc: CsvEncoder[T]
                                                ): CsvEncoder[H :+: T] =
  createEncoder {
    case Inl(h) => hEnc.value.encode(h)
    case Inr(t) => tEnc.encode(t)
  }

val shapes: List[Shape] = List(
  Rectangle(3.0,4.0),
  Circle(1.0)
)

implicit val doubleEncoder = createEncoder[Double](d => List(d.toString))
implicit val cnilEncoder: CsvEncoder[CNil] = createEncoder[CNil](cnil => throw new Exception("Inconceivable!"))

writeCsv(shapes)

writeCsv(List[Tree[Int]](Branch(Leaf(2), Leaf(4))))