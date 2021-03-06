package controllers

import play.api.Logger
import play.api.data.Forms._
import play.api.data._
import play.api.db.DB
import play.api.mvc._
import javax.inject.Inject
import anorm._
import play.api.Play.current

class Account @Inject() extends Controller with Secured {

  val profileFormTuple = Form(
    tuple(
      "name" -> text.verifying("Name required",!_.isEmpty),
      "email" -> text.verifying(FormConstraint.emailCheckConstraint)
    )
  )

  def profile = withAuth{ account=> implicit request=>
    val filledProfileFormTuple=profileFormTuple.fill((account.name,account.email))
    Ok(views.html.profile(filledProfileFormTuple))
  }

  def profilesubmit = withAuth{ account=> implicit request=>

    profileFormTuple.bindFromRequest().fold(
      formWithErrors =>{
        val error=formWithErrors.errors.foldLeft("")( (a,b)=>a+b.message+", " ).dropRight(2)
        Redirect(routes.Account.profile()).flashing("error"->error)
      },
      success=>{
        DB.withConnection{
          implicit c=>
            val rowsUpdated:Int=SQL("update users set name={name},email={email} where id={id}").on("name"->success._1,"email"->success._2, "id"->account.id).executeUpdate()
            if(rowsUpdated>0){
              Redirect(routes.Account.profile()).flashing("success"->"Save successfully")
            }else{
              Redirect(routes.Account.profile()).flashing("error"->"Error in saving")
            }
        }
      }
    )
  }

  def account  = withAuth{ account=> implicit request=>
    Ok(views.html.account())
  }


  def accountsubmit = withAuth { account => implicit request =>
    Form("password"->text.verifying("Password required",!_.isEmpty)).bindFromRequest().fold(
      formWithErrors =>{
        val error=formWithErrors.errors.foldLeft("")( (a,b)=>a+b.message+", " ).dropRight(2)
        Redirect(routes.Account.account()).flashing("error"->error)
      },
      password =>{
        DB.withConnection {
          implicit c =>
            val rowsUpdated: Int = SQL("update users set password={password} where id = {id}").on("password"->password,"id"->account.id).executeUpdate()
            if(rowsUpdated>0){
              Redirect(routes.Account.account()).flashing("success"->"Save successfully")
            }else{
              Redirect(routes.Account.account()).flashing("error"->"Error in saving")
            }
        }
      }
    )
  }

}
