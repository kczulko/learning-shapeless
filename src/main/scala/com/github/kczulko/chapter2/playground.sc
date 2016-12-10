import com.github.kczulko.chapter2.adts._
import shapeless.Generic

case class Red()
case class Amber()
case class Green()

val iceCreamGen = Generic[IceCream]
val iceCream = IceCream("Vanilla", 1, false)
val repr = iceCreamGen.to(iceCream)
val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

val genShape = Generic[Shape]

genShape.to(Circle(2.0))
genShape.to(Rectangle(2.0, 3.0))