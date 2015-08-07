package controllers

import play.api.db.DB
import play.api.mvc.{Action, RequestHeader}
import models.Account
import play.api.mvc._
import play.api.Play.current
import anorm._


trait Secured {

  implicit var usersfullname:String=""

  def username(request: RequestHeader) = request.session.get("email")

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)

  def withAuth(f: => models.Account => Request[AnyContent] => Result) = {

    Security.Authenticated(username,onUnauthorized){
      email=>
        DB.withConnection{
          implicit c=>
            val findUser=SQL("Select * from users where email={email}").on("email"->email).apply()
            if(findUser==Stream.empty) {
              Action(request=> onUnauthorized(request))
            }else {
              val user = findUser.head;

              usersfullname=user[String]("name")

              val account = new models.Account(user[String]("email"),user[String]("password"),user[String]("name"),user[Int]("id"))
              Action(request=>f(account)(request))
            }
        }

    }
  }


}
