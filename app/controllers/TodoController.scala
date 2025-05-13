package controllers

import javax.inject._ // DIのために必要
import play.api.mvc._
import play.api.libs.json._
import models.Todo // 作成したTodoモデルをインポート
import scala.collection.mutable.ListBuffer // インメモリデータストア用
import scala.concurrent.{ExecutionContext, Future} // 非同期処理用

@Singleton // このコントローラーがシングルトンであることを示す
class TodoController @Inject()(
    val controllerComponents: ControllerComponents
)(implicit ec: ExecutionContext) // ExecutionContextを暗黙的にインジェクト
    extends BaseController {

  // インメモリのTODOリスト (サーバー再起動で消えます)
  private val todoList = new ListBuffer[Todo]()
  // IDを生成するためのシンプルなカウンター
  private var nextId: Long = 1L

  // リクエストボディから受け取るためのデータ構造 (idはサーバー側で振るため入力には含めない)
  // completedは省略可能とし、省略された場合はfalseとする
  case class TodoInput(title: String, completed: Option[Boolean])
  implicit val todoInputFormat: Format[TodoInput] = Json.format[TodoInput]


  // GET /todos - 全てのTODOアイテムを取得
  def list(): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    // 現在のtodoListをJSONとして返す
    // Future.successfulでラップして非同期Actionの型に合わせる
    Future.successful(Ok(Json.toJson(todoList.toList)))
  }

  // POST /todos - 新しいTODOアイテムを作成
  def create(): Action[JsValue] = Action(parse.json).async { implicit request: Request[JsValue] =>
    // リクエストボディのJSONをTodoInputとしてパース・バリデーション
    request.body.validate[TodoInput].fold(
      // バリデーションエラーの場合
      errors => {
        Future.successful(BadRequest(Json.obj("message" -> "Invalid JSON", "details" -> JsError.toJson(errors))))
      },
      // バリデーション成功の場合
      todoInput => {
        val newTodo = Todo(
          id = nextId,
          title = todoInput.title,
          completed = todoInput.completed.getOrElse(false) // completedがNoneならfalse
        )
        todoList += newTodo
        nextId += 1
        Future.successful(Created(Json.toJson(newTodo))) // 201 Createdステータスで新しいTODOを返す
      }
    )
  }

  // GET /todos/:id - 特定のIDのTODOアイテムを取得
  def get(id: Long): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    todoList.find(_.id == id) match {
      case Some(todo) => Future.successful(Ok(Json.toJson(todo)))
      case None       => Future.successful(NotFound(Json.obj("message" -> s"Todo with id $id not found")))
    }
  }

  // PUT /todos/:id - 特定のIDのTODOアイテムを更新
  def update(id: Long): Action[JsValue] = Action(parse.json).async { implicit request: Request[JsValue] =>
    request.body.validate[TodoInput].fold(
      errors => {
        Future.successful(BadRequest(Json.obj("message" -> "Invalid JSON", "details" -> JsError.toJson(errors))))
      },
      todoInput => {
        todoList.indexWhere(_.id == id) match {
          case -1 => // 見つからない場合
            Future.successful(NotFound(Json.obj("message" -> s"Todo with id $id not found")))
          case index => // 見つかった場合
            val originalTodo = todoList(index)
            val updatedTodo = originalTodo.copy(
              title = todoInput.title, // titleは必須
              completed = todoInput.completed.getOrElse(originalTodo.completed) // completedは入力があれば更新
            )
            todoList(index) = updatedTodo
            Future.successful(Ok(Json.toJson(updatedTodo)))
        }
      }
    )
  }

  // DELETE /todos/:id - 特定のIDのTODOアイテムを削除
  def delete(id: Long): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    todoList.indexWhere(_.id == id) match {
      case -1 =>
        Future.successful(NotFound(Json.obj("message" -> s"Todo with id $id not found")))
      case index =>
        todoList.remove(index)
        // NoContent (204) を返すのが一般的だが、ここではメッセージ付きのOKを返す
        Future.successful(Ok(Json.obj("message" -> s"Todo with id $id deleted successfully")))
    }
  }
}