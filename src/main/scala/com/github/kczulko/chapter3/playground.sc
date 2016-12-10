import com.github.kczulko.chapter2.adts._
import shapeless.the

trait CsvEncoder[A] {
  def encode(a: A): List[String]
}

implicit object EmployeeCsvEncoder extends CsvEncoder[Employee] {
  override def encode(a: Employee): List[String] = List(
    a.name,
    a.id.toString,
    a.isManager.toString
  )
}

implicit object IceCreamCsvEncoder extends CsvEncoder[IceCream] {
  override def encode(a: IceCream): List[String] = List(
    a.name,
    a.numCherries.toString,
    a.inCone.toString
  )
}

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

println { writeCsv(employees zip iceCreams) }
