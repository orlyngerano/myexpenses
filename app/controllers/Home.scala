package controllers


import play.api.mvc._

class Home extends Controller with Secured{

  def dashboard = withAuth{ account=> implicit request=>
    Ok(views.html.dashboard())
  }

}
