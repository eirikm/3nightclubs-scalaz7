package nightclubs

/**
 * Part Zero : 10:15 Saturday Night
 *
 * (In which we will see how to let the type system help you handle failure)...
 *
 * First let's define a domain. (All the following requires scala 2.10.x and scalaz 7.0.x)
 */

import scalaz._
import Scalaz._


/**
 * Let's define a trait which will contain the checks that *all* nightclubs make!
 */
trait Nightclub {

  //First CHECK
  def checkAge(p: Person): Validation[String, Person] =
    if (p.age < 18)
      "Too Young!".fail
    else if (p.age > 40)
      "Too Old!".fail
    else
      p.success

  //Second CHECK
  def checkClothes(p: Person): Validation[String, Person] =
    if (p.gender == Gender.Male && !p.clothes("Tie"))
      "Smarten Up!".fail
    else if (p.gender == Gender.Female && p.clothes("Trainers"))
      "Wear high heels".fail
    else
      p.success

  //Third CHECK
  def checkSobriety(p: Person): Validation[String, Person] =
    if (Set(Sobriety.Drunk, Sobriety.Paralytic, Sobriety.Unconscious) contains p.sobriety)
      "Sober Up!".fail
    else
      p.success
}
