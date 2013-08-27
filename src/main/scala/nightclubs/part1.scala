package nightclubs

import scalaz._
import Scalaz._

/**
 * Part One : Clubbed to Death
 *
 * Now let's compose some validation checks
 *
 */
object ClubbedToDeath extends Nightclub {
  def costToEnter(p : Person) : Validation[String, Double] = {

    //PERFORM THE CHECKS USING Monadic "for comprehension" SUGAR
    for {
      a <- checkAge(p)
      b <- checkClothes(a)
      c <- checkSobriety(b)
    } yield
      if (c.gender == Gender.Female) 0D
      else 5D
  }
}



// Now let's see these in action
object ClubbedToDeathTest extends App {
  // Let's go clubbing!
  prettyPrint(Dave)(ClubbedToDeath.costToEnter)
  prettyPrint(Ken)(ClubbedToDeath.costToEnter)
  prettyPrint(Ruby)(ClubbedToDeath.costToEnter)
  prettyPrint(Ruby.copy(name = "Young Ruby", age = 17))(ClubbedToDeath.costToEnter)
  prettyPrint(Ken.copy(name = "Happy Ken", sobriety = Sobriety.Unconscious))(ClubbedToDeath.costToEnter)
}



/**
 * The thing to note here is how the Validations can be composed together in a for-comprehension.
 * Scala's type system is making sure that   failures flow through your computation in a safe manner.
 */
