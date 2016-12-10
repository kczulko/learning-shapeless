import shapeless._

case class IceCream(name: String, numCherries: Int, inCone: Boolean)
case class Employee(name: String, WWID: Int, isManager: Boolean)

case class Red()
case class Amber()
case class Green()

val iceCreamGen = Generic[IceCream]
val iceCream = IceCream("Vanilla", 1, false)
val repr = iceCreamGen.to(iceCream)
val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

type Light = Red :+: Amber :+: Green :+: CNil

val red: Light = Inl(Red())
val green: Light = Inr(Inr(Inl(Green())))

sealed trait Shape

final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double) extends Shape

val gen = Generic[Shape]

gen.to(Circle(2.0))
gen.to(Rectangle(2.0, 3.0))

def example(f: Int => Any): Unit = {
  print(f(5))
}

val g: (Any) => Int = v => v.hashCode()
example(g)

trait CsvEncoder[A] {
  def encode(a: A): List[String]
}

implicit object EmployeeCsvEncoder extends CsvEncoder[Employee] {
  override def encode(a: Employee): List[String] = List(
    a.name,
    a.WWID.toString,
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

object CsvEncoders {
  implicit def pairEncoder[A : CsvEncoder, B : CsvEncoder]: CsvEncoder[(A,B)] =
    a => implicitly[CsvEncoder[A]].encode(a._1) ++ implicitly[CsvEncoder[B]].encode(a._2)
}

def writeCsv[A : CsvEncoder](values: List[A]): String = {
  values.map(value => implicitly[CsvEncoder[A]].encode(value).mkString(",")).mkString("\n")
}

val employees = List(
  Employee("fdsa", 1234, true),
  Employee("fdsa", 1234, false)
)

val iceCreams = List(
  IceCream("vanilla", 123, false),
  IceCream("vanilla", 123, true)
)

//writeCsv(employees zip iceCreams)

case class Kczulko(bored: Boolean = true)

import CsvEncoders._

println { writeCsv(employees zip iceCreams) }
