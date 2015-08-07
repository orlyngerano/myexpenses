package controllers

import javax.inject.Inject

import play.api._
import play.api.data._
import play.api.db.DB
import play.api.i18n.{MessagesApi, I18nSupport}
import play.api.mvc._
import play.api.data.Forms._
import play.api.Play.current
import anorm._

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
      "email"-> email,
      "password"-> nonEmptyText
    ) verifying("Failed to validate from DB", fields=>fields match {
      case loginData => validate(loginData._1,loginData._2)
    })
  )


  def login = Action {
    implicit request =>

    Ok(views.html.login())
  }

  def loginsubmit = Action{

    implicit request =>

      var result:Result=NoContent

    loginForm.bindFromRequest.fold(
      formWithErrors =>{
        var error="";
        formWithErrors.errors.foreach(
          e=> error = error+e.key+" "+e.message+", "
        )
        result=Redirect(routes.Application.login()).flashing("error"->error.dropRight(2))
      },
      loginData => {
        val session:Session=request.session+("email"->loginData._1)
        result=Redirect(routes.Home.dashboard()).withSession(session)
      }
    )
    result
  }

  def index = Action {
    Redirect(routes.Application.login())
  }



  def logout = Action{
    Redirect(routes.Application.login()).withNewSession
  }


}
