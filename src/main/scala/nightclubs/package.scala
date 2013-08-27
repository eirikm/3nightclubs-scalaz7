import scalaz._
import Scalaz._

package object nightclubs {

  object Sobriety extends Enumeration {
    val Sober, Tipsy, Drunk, Paralytic, Unconscious = Value }

  object Gender extends Enumeration {
    val Male, Female = Value }

  case class Person(name: String,
                    gender: Gender.Value,
                    age: Int,
                    clothes: Set[String],
                    sobriety: Sobriety.Value)

  type Validated[A] = ValidationNel[String, A]

  val Dave = Person("Dave", Gender.Male, 41, Set("Tie", "Jeans"), Sobriety.Sober)
  val Ken = Person("Ken", Gender.Male, 28, Set("Tie", "Shirt"), Sobriety.Tipsy)
  val Ruby = Person("Ruby", Gender.Female, 25, Set("High Heels"), Sobriety.Tipsy)
}
