package nightclubs

import scalaz._
import Scalaz._

/**
 * with type lambdas and function composition
 */
object GayBikerBar extends Nightclub {

  def checkGender(p : Person) : Validation[String, Person] =
    if (p.gender != Gender.Male)
      "Men Only".fail
    else
      p.success

  def costToEnter(p : Person) : ValidationNel[String, Double] = {
    val checks = List(checkAge _, checkClothes _, checkSobriety _, checkGender _)
    (checks map {(_ : (Person => Validation[String, Person])).apply(p).toValidationNel}).sequence[({type l[a]=ValidationNel[String, a]})#l, Person] map {
      case c :: _ => c.age + 1.5D
    }
  }

  // Interestingly, as traverse is basically map + sequence, we can reduce this even further
  def costToEnter2(p : Person) : ValidationNel[String, Double] = {
    val checks = List(checkAge _, checkClothes _, checkSobriety _, checkGender _)
    checks.traverse[({type l[a] = ValidationNel[String, a]})#l, Person](_ andThen (_.toValidationNel) apply p) map { case c :: _ => c.age + 1.5D }
  }
}

object GayBikerBarTest extends App {
  prettyPrint(Person("Bill Gates", Gender.Male, 59, Set("Jeans"), Sobriety.Paralytic))(GayBikerBar.costToEnter)
  prettyPrint(Person("Bill Gates", Gender.Male, 59, Set("Jeans"), Sobriety.Paralytic))(GayBikerBar.costToEnter)
}
