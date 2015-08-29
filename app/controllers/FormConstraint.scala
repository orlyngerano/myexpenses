package controllers

import play.api.data.validation.{Invalid, Valid, ValidationError, Constraint}

import scala.util.matching.Regex

/**
 * Created by orlyngerano on 8/29/15.
 */
object FormConstraint {

  def emailCheckConstraint:Constraint[String] = Constraint("constraints.emailcheck")({
    plainText =>
      val validEmailPattern =  """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r
      val errors = plainText match {
        case "" => Seq(ValidationError("Email required"))
        case validEmailPattern() => Nil
        case _ => Seq(ValidationError("Email invalid"))
      }

      if (errors.isEmpty) {
        Valid
      } else {
        Invalid(errors)
      }
  })
}
