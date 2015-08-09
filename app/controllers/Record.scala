package controllers

import java.util.{Calendar, Date}

import play.api.data.Form
import play.api.data.Forms._
import play.api.db.DB
import play.api.libs.json._
import play.api.mvc._
import anorm._
import play.api.Play.current

class Record extends Controller with Secured{

  val recordFormTuple = Form(
    tuple(
      "id"->number,
      "cost" -> nonEmptyText,
      "name" -> nonEmptyText,
      "boughtdate" -> nonEmptyText,
      "details" -> nonEmptyText
    )
  )

  def records = withAuth{ account=> implicit request=>
    Ok(views.html.records())
  }

  def record(id: Int) = withAuth{ account=> implicit request=>

    var filledRecordFormTuple=recordFormTuple.fill((id,"","","",""))

    DB.withConnection{
      implicit c=>

        val findExpense=SQL("Select * from expenses where id={id}").on("id"->id).apply()
        if(findExpense!=Stream.empty) {
          val expense = findExpense.head;
          val expenseMap= new Tuple5[Int,String,String,String,String](expense[Int]("id"),expense[String]("cost"),expense[String]("name"),expense[String]("boughtdate"),expense[String]("details"))
          filledRecordFormTuple=recordFormTuple.fill(expenseMap)
        }
    }

    Ok(views.html.record(filledRecordFormTuple))
  }

  def recordsubmit = withAuth{ account=> implicit request=>

    recordFormTuple.bindFromRequest().fold(
      hasErrors=>{
        var error="";
        hasErrors.errors.foreach(
          e=> error = error+e.key+" "+e.message+", "
        )
        Redirect(routes.Record.record(hasErrors.data.get("id").get.toInt)).flashing("error"->error.dropRight(2))
      },
      success=>{
        DB.withConnection{
          implicit c=>
            if(success._1==0){
              val id: Option[Long]=SQL("insert into expenses(cost,name,boughtdate,details) values({cost},{name},{boughtdate},{details})").on("cost"->success._2,"name"->success._3, "boughtdate"->success._4, "details"->success._5).executeInsert()
              if(!id.isEmpty){
                Redirect(routes.Record.record(id.get.asInstanceOf[Int])).flashing("success"->"Save successfully")
              }else{
                Redirect(routes.Record.record(success._1)).flashing("error"->"Error in saving")
              }
            }else{
              val rowsUpdated:Int=SQL("update expenses set cost={cost},name={name},boughtdate={boughtdate},details={details} where id={id}").on("cost"->success._2,"name"->success._3, "boughtdate"->success._4, "details"->success._5, "id"->success._1).executeUpdate()
              if(rowsUpdated>0){
                Redirect(routes.Record.record(success._1)).flashing("success"->"Save successfully")
              }else{
                Redirect(routes.Record.record(success._1)).flashing("error"->"Error in saving")
              }
            }
        }
      }
    )

  }

  def recordsjson(page:Int,search:Option[String],filter:Option[String]) = Action{
    implicit request=>

      DB.withConnection {
        implicit c =>

          val sqlRecordsToJson = (records:Stream[Row]) => {

            var expensesJson = JsArray()
            records.foreach(row=>{
              expensesJson=expensesJson.append(JsObject(Seq(
                "id"->JsNumber(row[Int]("id")),
                "cost"->JsString(row[String]("cost")),
                "name"->JsString(row[String]("name")),
                "boughtdate"->JsString(row[String]("boughtdate")),
                "details"->JsString(row[String]("details"))
              )))
            })
            expensesJson
          }

          val limit = 5
          val offset = (page - 1) * limit

          val getJsonRecordsWithFilter = (search: String, filter: String) => {
            Map(
              "records" ->
              sqlRecordsToJson{
                  SQL(s"select * from expenses where $filter like '%$search%' order by id desc limit $limit offset $offset").apply()
              },
              "countRecords" -> {
                var countRow = SQL(s"select count(*) as c from expenses where $filter like '%$search%'").apply().head
                JsNumber(countRow[Long]("c"))
              }
            )
          }

          val getJsonRecords = {
            Map(
              "records" ->
               sqlRecordsToJson {
                  SQL"select * from expenses order by id desc limit $limit offset $offset".apply()
               },
              "countRecords" -> {
                var countRow = SQL"Select count(*) as c from expenses".apply().head
                JsNumber(countRow[Long]("c"))
              }
            )
          }

          val records = search match {
            case Some(s) => {
              filter match {
                case Some("name") => Some(getJsonRecordsWithFilter(s, "name"))
                case Some("cost") => Some(getJsonRecordsWithFilter(s, "cost"))
                case Some("date") => Some(getJsonRecordsWithFilter(s, "boughtdate"))
                case Some("details") => Some(getJsonRecordsWithFilter(s, "details"))
                case _ => None
              }
            }
            case _ => Some(getJsonRecords)
          }


          val jsonRecords = records match {
            case Some(s) => {
              JsObject(Seq(
                "total"->s.get("countRecords").get,
                "per_page"->JsNumber(limit),
                "current_page"->JsNumber(page),
                "data"->s.get("records").get
              ))
            }
            case _ => {
              JsObject(Seq(
                "total"->JsNumber(0),
                "per_page"->JsNumber(0),
                "current_page"->JsNumber(0),
                "data"->JsArray()
              ))
            }
          }

          Ok(Json.toJson(jsonRecords))
      }

  }

  def recorddelete  = Action {
    implicit request=>

      Form("id"->number).bindFromRequest().fold(
        hasErrors =>{
          Ok("Error")
        },
        id =>{
          DB.withConnection {
            implicit c =>
              val result: Int = SQL("delete from expenses where id = {id}").on("id"->id).executeUpdate()
              if(result>0){
                Ok("Ok")
              }else{
                Ok("Error")
              }
          }
        }
      )
  }


  def expensejson(startdate:String,enddate:String) = Action{
    implicit request=>

      val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
      var startDate = dateFormat.parse(startdate)
      val endDate = dateFormat.parse(enddate)
      val calendar = Calendar.getInstance();
      var expensesJson=JsArray()

      DB.withConnection {
        implicit c =>
          val selectExpenses = SQL("select sum(cost) as costtotal,boughtdate from expenses group by boughtdate")
          val expensesList:List[(String,Int)] = selectExpenses().map(row =>
            row[String]("boughtdate")->row[Int]("costtotal")
          ).toList

/*
          expensesList.foreach(r=>{
            println(r._2)
          })
*/



          while(startDate.compareTo(endDate)<1){
            val date = dateFormat.format(startDate)
            val expenses = expensesList.find{ a=> dateFormat.parse(a._1).compareTo(dateFormat.parse(date))==0 } match {
              case None => 0
              case x=> x.get._2
            }

            expensesJson = expensesJson.:+(JsObject(
              Seq("date"->JsString(date),"expenses"->JsNumber(expenses))
            ))
            calendar.setTime(startDate)
            calendar.add(Calendar.DAY_OF_MONTH,1)
            startDate=calendar.getTime
          }
      }

      Ok(Json.toJson(expensesJson))
  }


}
