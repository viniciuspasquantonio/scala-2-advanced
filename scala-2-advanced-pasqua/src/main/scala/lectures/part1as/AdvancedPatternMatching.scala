package lectures.part1as

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val descriptuib = numbers match {
    case head :: Nil => println(s"The only element is $head.")
    case _ =>
  }

  /*
  - constants
  -wildcards
  -case classes
  - tuples
  -some special magic like above
   */

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None else Some((person.name, person.age))

    def unapply(age: Int): Option[String] = Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match {
    case Person(n, a) => s"Hi, my name is $n and I am $a years old."
  }

  println(greeting)

  val legalStatus = bob.age match {
    case Person(status) => s"My legal status is $status."
  }

  println(legalStatus)


  /*
    Exercise.
   */

  val n : Int = 45
  val mathProperty = n match {
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => "an even number"
    case _ => "no property"
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg < 10
  }

  object even {
    def unapply(arg: Int): Boolean = arg % 2 != 0
  }

  val m: Int = 8
  val mathProperty2 = m match {
    case singleDigit() => "single digit"
    case even() => "an even number"
    case _ => "no property"
  }

  println(mathProperty2)
}
