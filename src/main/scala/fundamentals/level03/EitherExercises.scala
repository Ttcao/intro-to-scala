package fundamentals.level03

import fundamentals.level02.TypesExercises.{Person, TrafficLight}
import fundamentals.level02.TypesExercises.TrafficLight._

/**
  * These exercises are optional and mirror the safe constructor exercises from `OptionExercises1.scala`
  */
object EitherExercises {

  case class AppError(msg: String)

  /**
    * scala> mkTrafficLightEither("red")
    * = Right(Red)
    *
    * scala> mkTrafficLightEither("bob")
    * = Left(AppError("bob is not a valid traffic light"))
    **/
  def mkTrafficLightEither(str: String): Either[AppError, TrafficLight] = {
    str match {
      case "red" => Right(Red)
      case "green" => Right(Green)
      case "yellow" => Right(Yellow)
      case other => Left(AppError(s"$other is not a valid traffic light"))
    }
  }

  /**
    * scala> mkTrafficLightEitherThenShow("red")
    * = "Traffic light is red"
    *
    * scala> mkTrafficLightEitherThenShow("bob")
    * = "bob is not a valid traffic light"
    *
    * Hint: Use `mkTrafficLightEither` and pattern matching
    */
  def mkTrafficLightEitherThenShow(str: String): String = {
    mkTrafficLightEither(str) match {
      case Right(Red) => "Traffic light is red"
      case Right(Green) => "Traffic light is green"
      case Right(Yellow) => "Traffic light is yellow"
      case Left(err) => s"${err.msg}"
    }
  }

  /**
    * scala> mkPersonEither("Bob", 22)
    * = Right(Person("Bob", 22))
    *
    * scala> mkPersonEither("Bob", -1)
    * = Left(AppError("Age cannot be less than zero: -1"))
    *
    * scala> mkPersonEither("", 22)
    * = Left(AppError("Name cannot be empty"))
    **/
  def mkPersonEither(name: String, age: Int): Either[AppError, Person] = {
    (name, age) match {
      case (n, a) if n.nonEmpty && a >= 0 => Right(Person(n,a))
      case (n, _) if n.isEmpty => Left(AppError("Name cannot be empty"))
      case (_, a) if a < 0 => Left(AppError(s"Age cannot be less than zero: $a"))
      case _ => Left(AppError("Name and age is invalid"))
    }
  }

}
