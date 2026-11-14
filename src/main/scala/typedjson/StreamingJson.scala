package typedjson

import io.circe.{Json, parser}

object StreamingJson:
  def renderTransaction(transaction: Transaction): Json =
    Json.obj(
      "date" -> Json.fromString(transaction.date.toString),
      "category" -> Json.fromString(transaction.category.value),
      "amount" -> Json.fromBigDecimal(transaction.amount),
      "note" -> transaction.note.fold(Json.Null)(Json.fromString)
    )
