package controllers

import javax.inject.{Singleton, Inject}

import akka.actor.ActorSystem
import akka.util.Timeout
import myactors.UserActor
import play.api.data._
import play.api.mvc._
import play.api.data.Forms._

@Singleton
class Application@Inject() (system: ActorSystem) extends Controller{

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import scala.concurrent.duration._
  import akka.pattern.ask
  implicit val timeout:Timeout = 5.seconds

  val userActor = system.actorOf(UserActor.props,"user-actor")

  val loginForm = Form(
    tuple(
      "email"->email,
      "password"->nonEmptyText()
    )
  )

  def login = Action {
    implicit request =>
    Ok(views.html.login())
  }

  def loginsubmit = Action.async{
    implicit request =>
      loginForm.bindFromRequest.fold(
        formWithErrors =>{
           scala.concurrent.Future {
            formWithErrors.errors.foldLeft("")( (a,b)=>a+b.message+", " ).dropRight(2)
          }.map( e =>
            Redirect(routes.Application.login()).flashing("error"->e)
            )
        },
        loginData => {
          (userActor? UserActor.Authenticate(loginData._1,loginData._2)).mapTo[Boolean].map(ok=>
            if(ok){
              val session:Session=request.session+("email"->loginData._1)
              Redirect(routes.Home.dashboard()).withSession(session)
            }else{
              Redirect(routes.Application.login()).flashing("error"->"Authentication failed!")
            }
          )
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
