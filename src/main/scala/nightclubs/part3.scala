package nightclubs

import scalaz._
import Scalaz._

/**
 *
 * Part Three : Gay Bar
 *
 * And for those wondering how to do this with a *very long list* of checks. Use sequence:
 *   List[ValidationNEL[E, A]] ~> (via sequence) ~> ValidationNEL[E, List[A]]
 *
 * Here we go:
 *
 */
object GayBar extends Nightclub {
  def checkGender(p : Person) : Validation[String, Person] =
    if (p.gender != Gender.Male)
      "Men Only".fail
    else
      p.success

  val checks: List[(Person) => Validation[String, Person]] =
    List(checkAge _, checkClothes _, checkSobriety _, checkGender _)

  def costToEnter(p : Person) : Validated[Double] = {
    val listOfValidatedPersons: List[Validated[Person]] =
      checks map {
        f => f(p).toValidationNel
      }

    val validatedListOfPersons: Validated[List[Person]] =
      listOfValidatedPersons.sequence[Validated, Person]

    validatedListOfPersons map {
      case c :: _ => c.age + 1.5D
    }
  }

  // Interestingly, as traverse is basically map + sequence, we can reduce this even further
  def costToEnter2(p : Person) : ValidationNel[String, Double] = {
    val validatedListOfPersons: Validated[List[Person]] =
      checks.traverse[Validated, Person](f => f(p).toValidationNel)

    validatedListOfPersons map { case c :: _ => c.age + 1.5D }
  }
}

object GayBarTest extends App {
  def prettyPrintCostToEnter(p: Person) {
    println(s"${p.name}: ${p} - ${GayBar.costToEnter(p)}")
  }


  prettyPrintCostToEnter(Person("Bill Gates", Gender.Male, 59, Set("Jeans"), Sobriety.Paralytic))
  prettyPrintCostToEnter(Person("Bill Gates", Gender.Male, 59, Set("Jeans"), Sobriety.Paralytic))
}

/**
 * As always; the point is that our validation functions are "static";
 * we do not need to change the way they have been coded because we want to combine them in different ways
 */
