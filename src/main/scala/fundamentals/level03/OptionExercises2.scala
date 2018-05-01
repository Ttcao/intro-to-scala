package fundamentals.level03

/**
  * These exercises simulate the "real-world" problem of retrieving records from a data store. You will learn to use `Option`s to represent
  * values that may or may not exist in the data store and also techniques to work with the `Option` type.
  */
object OptionExercises2 {

  type JobId = Int

  type HumanId = Int

  case class Job(name: String, description: String)

  case class Human(name: String, optJobId: Option[JobId])

  val jobsDatabase: Map[JobId, Job] = Map(
    1 -> Job("Teacher", "Expert in their field"),
    2 -> Job("Engineer", "Build things for people")
  )

  val humansDatabase: Map[HumanId, Human] = Map(
    1 -> Human("Sally", None),
    2 -> Human("Jenny", Some(1)),
    3 -> Human("Timmy", Some(1024)) // jobId doesn't exist in jobsDatabase
  )

  /**
    * scala> findHumanById(1)
    * = Some(Human("Sally", None))
    *
    * scala> findHumanById(100)
    * = None
    *
    * Hint: use get method on Map
    **/
  def findHumanById(humanId: HumanId): Option[Human] = {
    humanId match {
      case 1 => Some(Human("Sally", None))
      case 2 => Some(Human("Jenny", Some(1)))
      case 3 => Some(Human("Timmy", Some(1024)))
      case _ => None
    }
  }

  /**
    * scala> findJobById(1)
    * = Some(Job("Teacher", "Expert in their field"))
    *
    * scala> findJobById(100)
    * = None
    **/
  def findJobById(jobId: JobId): Option[Job] = {
    jobId match {
      case 1 => Some(Job("Teacher", "Expert in their field"))
      case 2 => Some(Job("Engineer", "Build things for people"))
      case _ => None
    }
  }

  /**
    * scala> findJobDescriptionGivenJobId1(1)
    * = Some("Expert in their field")
    *
    * scala> findJobDescriptionGivenJobId1(100)
    * = None
    *
    * Hint: Use pattern matching
    */
  def findJobDescriptionGivenJobId1(jobId: JobId): Option[String] = {
    jobId match {
      case 1 => Some("Expert in their field")
      case 2 => Some("Build things for people")
      case _ => None
    }
  }

  /**
    * Same as above, but use .map instead
    *
    * If you see a `case None => None` in a pattern matching block,
    * you can always refactor it:
    *
    * ```
    * optSomething match {
    *   case Some(a) => Some(s"Got some $a")
    *   case None => None
    * }
    * ```
    *
    * becomes
    *
    * ```
    * optSomething.map(a => s"Got some $a")
    * ```
    */
  def findJobDescriptionGivenJobId2(jobId: JobId): Option[String] = {
    findJobById(jobId).map(job => job.description)
  }

  /**
    * scala> findJobDescriptionGivenJobIdOrElse1(1)
    * = "Expert in their field"
    *
    * scala> findJobDescriptionGivenJobIdOrElse1(100)
    * = "Job with id 100 does not exist"
    *
    * Hint: Use pattern matching
    */
  def findJobDescriptionGivenJobIdOrElse1(jobId: JobId): String =
    findJobDescriptionGivenJobId2(jobId) match {
      case Some(description) => description
      case None => s"Job with id $jobId does not exist"
    }

  /**
    * Same as above, but use .map then .getOrElse
    */
  def findJobDescriptionGivenJobIdOrElse2(jobId: JobId): String = {
    findJobDescriptionGivenJobId2(jobId).map(desc => desc).getOrElse(s"Job with id $jobId does not exist")
  }

  /**
    * scala> findJobIdByHumanId(1)
    * = None
    *
    * scala> findJobIdByHumanId(2)
    * = Some(1)
    *
    * Hint: Try .map, .flatten
    *
    * What's the type that you get after using .map? What's different between that and what the function return type is?
    */
  def findJobIdByHumanId(humanId: HumanId): Option[JobId] = findHumanById(humanId).map(human => human.optJobId).flatten

  /**
    * scala> findJobByHumanId(2)
    * = Some(Job("Teacher", "Expert in their field"))
    *
    * Hint: Use findJobIdByHumanId
    */
  def findJobByHumanId(humanId: HumanId): Option[Job] = findJobIdByHumanId(humanId).map(j => findJobById(j)).flatten

}