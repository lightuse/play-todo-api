package models

import play.api.libs.json._ // Play JSONライブラリをインポート

// TODOタスクを表すケースクラス
case class Todo(id: Long, title: String, completed: Boolean)

// コンパニオンオブジェクトでJSONフォーマッタを定義
object Todo {
  // TodoケースクラスとJSONを相互変換するための暗黙的なフォーマッタ
  // Json.format[Todo] マクロが自動的にReadsとWritesを生成します
  implicit val todoFormat: Format[Todo] = Json.format[Todo]
}