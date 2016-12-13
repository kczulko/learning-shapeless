import com.github.kczulko.chapter2.adts.{IceCream, Shape}
import shapeless.syntax.singleton._
import shapeless.labelled.{FieldType, KeyTag, field}
import shapeless.{HNil, LabelledGeneric, Witness, the}

trait Cherries

val numCherries = "specificName" ->> 12345

field[Cherries](123)

def getFieldName[K,V](value: FieldType[K,V])(implicit witness: Witness.Aux[K]): K =
 witness.value

getFieldName(numCherries)

def getFieldValue[K,V](value: FieldType[K,V]): V = value

getFieldValue(numCherries)

val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil

////////////////////////////////////////////////////
// JSON ENCODER
////////////////////////////////////////////////////

sealed trait JsonValue
case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
case class JsonArray(items: List[JsonValue]) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonNumber(value: Double) extends JsonValue
case class JsonBoolean(value: Boolean) extends JsonValue
case object JsonNull extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}

object JsonEncoder {
  def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
}

def createEncoder[A](func: A => JsonValue): JsonEncoder[A] = (value: A) => func(value)

implicit val stringEncoder = createEncoder[String](JsonString)
implicit val booleanEncoder = createEncoder[Boolean](JsonBoolean)
implicit val numberEncoder = createEncoder[Double](JsonNumber)
implicit val intEncoder = createEncoder[Int](JsonNumber(_))

implicit def listEncoder[A : JsonEncoder]: JsonEncoder[List[A]] =
  createEncoder[List[A]](list => JsonArray(list.map(the[JsonEncoder[A]].encode)))

implicit def optionEncoder[A : JsonEncoder]: JsonEncoder[Option[A]] =
  createEncoder[Option[A]](op => op.map(the[JsonEncoder[A]].encode).getOrElse(JsonNull))

val iceCream = IceCream(name = "Sundae", numCherries = 1, inCone = false)

val gen = LabelledGeneric[IceCream].to(iceCream)

trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  override def encode(value: A): JsonObject
}

def createObjectEncoder[A](f: A => JsonObject): JsonObjectEncoder[A] = f(_)

import shapeless.{HNil, ::, HList, Lazy}

implicit val hnilEncoder = createObjectEncoder[HNil](_ => JsonObject(Nil))

implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
                                              implicit
                                              witness: Witness.Aux[K],
                                              hEncoder: Lazy[JsonEncoder[H]],
                                              tEncoder: JsonObjectEncoder[T]
                                              ): JsonObjectEncoder[FieldType[K, H] :: T] = {
  val fieldName = witness.value.name
  createObjectEncoder { hlist =>
    val head = hEncoder.value.encode(hlist.head)
    val tail = tEncoder.encode(hlist.tail)
    JsonObject((fieldName, head) :: tail.fields)
  }
}

implicit def genericObjectEncoder[A, H <: HList](
                                                implicit
                                                generic: LabelledGeneric.Aux[A, H],
                                                hEncoder: Lazy[JsonObjectEncoder[H]]
                                                ): JsonEncoder[A] =
  createObjectEncoder { value =>
    hEncoder.value.encode(generic.to(value))
  }

JsonEncoder[IceCream].encode(iceCream)
