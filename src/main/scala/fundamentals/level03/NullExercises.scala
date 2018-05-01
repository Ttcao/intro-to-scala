package fundamentals.level03

import fundamentals.level02.TypesExercises.{Person, TrafficLight}
import TrafficLight._
import fundamentals.level02.TypesExercises

/**
  * These exercises are intended to show the problems that come with programming with `null`s.
  *
  * After these exercises, we will learn the alternative to using `null`s.
  */
object NullExercises {

  /**
    * scala> mkTrafficLightOrNull("red")
    * = Red
    *
    * scala> mkTrafficLightOrNull("bob")
    * = null
    **/
  def mkTrafficLightOrNull(str: String): TrafficLight = {
    str match {
      case "red" => Red
      case "green" => Green
      case "yellow" => Yellow
      case _ => null
    }
  }

  /**
    * scala> mkTrafficLightOrNullThenShow("red")
    * = "Traffic light is red"
    *
    * scala> mkTrafficLightOrNullThenShow("bob")
    * = "Traffic light is invalid"
    *
    * Hint: Use `mkTrafficLightOrNull` and pattern matching
    */
  def mkTrafficLightOrNullThenShow(str: String): String = {
    mkTrafficLightOrNull(str) match {
      case Red => "Traffic light is red"
      case Green => "Traffic light is green"
      case Yellow => "Traffic light is yellow"
      case _ => "Traffic light is invalid"
    }
  }

  /**
    * scala> mkPersonOrNull("Bob", 20)
    * = Person("Bob", 20)
    *
    * If `name` is blank:
    *
    * scala> mkPersonOrNull("", 20)
    * = null
    *
    * If `age` < 0:
    *
    * scala> mkPersonOrNull("Bob", -1)
    * = null
    **/
  def mkPersonOrNull(name: String, age: Int): Person = {
    if (name.trim.isEmpty || age < 1) null else Person(name, age)
  }

  /**
    * scala> mkPersonOrNullThenChangeName("Bob", 20, "John")
    * = Person("John", 20)
    *
    * scala> mkPersonOrNullThenChangeName("Bob", -1, "John")
    * = null
    *
    * For simplicity, let's allow changing to an empty name, like:
    *
    * scala> mkPersonOrNullThenChangeName("Bob", 20, "")
    * = Person("", 20)
    *
    * Hint: Use `mkPersonOrNull` and `changeName`
    **/
  def mkPersonOrNullThenChangeName(oldName: String, age: Int, newName: String): Person = {
    val person = mkPersonOrNull(oldName, age)
    if (person == null) {
      null
    }
    else {
      TypesExercises.changeName(newName, person)
    }

  }

  /**
    * Does the following function return a `null`?
    */
  def mean(nums: List[Int]): Double = ???

}
