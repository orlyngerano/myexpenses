package myactors

import akka.actor.{Props, Actor}
import akka.actor.Actor.Receive
import play.api.db.DB
import anorm._
import play.api.Play.current

object UserActor {
  def props = Props[UserActor]

  case class Authenticate(email:String, password:String)
}

class UserActor extends Actor {

  import UserActor.Authenticate

  def receive ={
    case Authenticate(email:String, password:String) =>
      var result:Boolean=false
      DB.withConnection{
        implicit c=>
          val findUser=SQL("Select * from users where email={email} and password={password}").on("email"->email,"password"->password).apply()
          if(findUser!=Stream.empty) {
            result=true
          }
      }
      sender() ! result
  }
}
