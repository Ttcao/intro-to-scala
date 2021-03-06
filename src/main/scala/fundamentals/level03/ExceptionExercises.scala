package fundamentals.level03

import fundamentals.level02.TypesExercises.Person

import scala.util.{Failure, Success, Try}

/**
  * These exercises are intended to show the difficulty of working with Exceptions.
  *
  * We will work through a better alternative to Exceptions after this.
  */
object ExceptionExercises {

  //Exceptions that will be thrown
  class EmptyNameException(message: String) extends Exception(message)

  class InvalidAgeValueException(message: String) extends Exception(message)

  class InvalidAgeRangeException(message: String) extends Exception(message)

  //test data of names and age pairs
  val personStringPairs =
    List(("Tokyo", "30"),
      ("Moscow", "5o"),
      ("The Professor", "200"),
      ("Berlin", "43"),
      ("Arturo Roman", "0"),
      ("", "30"))

  /**
    * Handling validation using Exceptions will come naturally if you are coming
    * to Scala from languages like Java or Ruby. In Scala there is a better way
    * to handle these scenarios, which we will get into later. For now let's
    * use Exceptions to handle the following scenarios and see where the
    * pain points lie.
    */

  /**
    * Implement the function getName, so that it either accepts the supplied name
    * and returns it unchanged or throws a EmptyNameException if the supplied name
    * is empty.
    *
    * scala> getName("Fred")
    * = "Fred"
    *
    * scala> getName("")
    * = EmptyNameException: provided name is empty
    *
    * scala> getName("   ")
    * = EmptyNameException: provided name is empty
    *
    * Hint: use the trim method on String to remove empty spaces
    */
  def getName(providedName: String): String =
    if (providedName.trim().isEmpty) throw new EmptyNameException("provided name is empty") else providedName

  /**
    * Implement the function getAge, so that it either accepts the supplied age
    * and returns it as an Int.
    * If the age can't be converted to an Int, throw an InvalidAgeValueException
    * If the provided age is not between 1 and 120 throw an InvalidAgeRangeException.
    *
    * scala> getAge("Fred")
    * = InvalidAgeValueException: provided age is invalid: Fred
    *
    * scala> getAge("20")
    * = 20
    *
    * scala> getAge("-1")
    * = InvalidAgeRangeException: provided age should be between 1-120: -1
    *
    * Hint: use the toInt method to convert a String to an Int.
    */

  def getAge(providedAge: String): Int = {
    try {
      if (providedAge.toInt < 1 || providedAge.toInt > 120)
        throw new InvalidAgeRangeException(s"provided age should be between 1-120: $providedAge")
      else
        providedAge.toInt
    } catch {
      case _: NumberFormatException => throw new InvalidAgeValueException(s"provided age is invalid: $providedAge")
    }
  }

  /**
    * Implement the function createPerson, so that it either accepts a name and age
    * and returns a Person instance or throws an EmptyNameException, InvalidAgeValueException or
    * InvalidAgeRangeException when given invalid values.
    *
    * Notice that createPerson is not declared to throw any Exceptions but the compiler does not complain.
    * What does this imply?
    *
    * scala> createPerson("Fred", "32")
    * = "Person(Fred, 32)"
    *
    * scala> createPerson("", "32")
    * = EmptyNameException: provided name is empty
    *
    * scala> createPerson("Fred", "ThirtyTwo")
    * = InvalidAgeValueException: provided age is invalid: ThirtyTwo
    *
    * scala> createPerson("Fred", "150")
    *
    * = InvalidAgeRangeException: provided age should be between 1-120: 150"
    **/
  def createPerson(name: String, age: String): Person = {
    Person(getName(name), getAge(age))
  }

  /**
    * Implement the function validPairs, that uses the personStringPairs List
    * and only returns valid pairs for name and age. It should not throw any Exceptions.
    *
    * scala> validPairs
    * = "List(("Tokyo", "30"), ("Berlin", "43"))
    *
    * Hint: use filter on List using the getName and getAge functions
    *
    */
  def validPairs: List[(String, String)] = {
    personStringPairs.filter(
      { case (name, age) => Try(getName(name)).isSuccess && Try(getAge(age).toString).isSuccess }
    )
  }

  /**
    * Implement the function createValidPeople that uses the validPairs function
    * to create a List of Person instances. It should not throw any Exceptions.
    *
    * scala> createValidPeople
    * = List(Person("Tokyo", 30), Person("Berlin", 43))
    *
    * Hint: use the output of validPairs and the createPerson function
    *
    */
  def createValidPeople: List[Person] = {
    validPairs.map { case (name, age) => Person(name, age.toInt) }
  }

  /**
    * Implement the function createValidPeople2 that only uses the collect function on List
    * to create a List of Person instances from personStringPairs. It should not throw any Exceptions.
    *
    * scala> createValidPeople2
    * = List(Person("Tokyo", 30), Person("Berlin", 43))
    *
    * What issues do you run into (if any)?
    *
    */
  def createValidPeople2: List[Person] = {
    personStringPairs.map { case (name, age) => (Try(getName(name)), Try(getAge(age))) }
      .collect { case (Success(n), Success(a)) => Person(n, a) }
  }

  /**
    * Implement the function collectErrors that collects all the Exceptions
    * that occur while processing personStringPairs.
    *
    * scala> collectErrors
    * = List(InvalidAgeValueException: provided age is invalid: 5o,
    * InvalidAgeRangeException: provided age should be between 1-120: 200,
    * InvalidAgeRangeException: provided age should be between 1-120: 0,
    * EmptyNameException: provided name is empty)
    *
    * What issues do you run into (if any)?
    *
    */
  def collectErrors: List[Exception] = {
    personStringPairs.map { case (name, age) => (Try(getName(name)), Try(getAge(age))) }
      .collect { case (Failure(ex: Exception), Failure(_: Exception)) => ex }
  }
}
