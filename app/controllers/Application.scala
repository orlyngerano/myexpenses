package controllers

import java.util.regex.Pattern
import javax.inject.Inject

import play.api._
import play.api.data._
import play.api.data.validation._
import play.api.db.DB
import play.api.i18n.{MessagesApi, I18nSupport}
import play.api.mvc._
import play.api.data.Forms._
import play.api.Play.current
import anorm._

import scala.util.matching.Regex

class Application extends Controller{



  def validate(email:String, password:String)={
    var result:Boolean=false

    DB.withConnection{
      implicit c=>
        val findUser=SQL("Select * from users where email={email} and password={password}").on("email"->email,"password"->password).apply()
        if(findUser!=Stream.empty) {
          result=true
        }
    }
    result
  }

  val loginForm = Form(
    tuple(
      "email"-> text.verifying(FormConstraint.emailCheckConstraint),
      "password"-> text.verifying("Password required",!_.isEmpty)
    ) verifying("Login failed", fields=>fields match {
      case loginData => validate(loginData._1,loginData._2)
    })
  )


  def login = Action {
    implicit request =>
    Ok(views.html.login())
  }

  def loginsubmit = Action{
    implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors =>{
        val error=formWithErrors.errors.foldLeft("")( (a,b)=>a+b.message+", " ).dropRight(2)
        Redirect(routes.Application.login()).flashing("error"->error)
      },
      loginData => {
        val session:Session=request.session+("email"->loginData._1)
        Redirect(routes.Home.dashboard()).withSession(session)
      }
    )
  }

  def index = Action {
    Redirect(routes.Application.login())
  }



  def logout = Action{
    Redirect(routes.Application.login()).withNewSession
  }


}
