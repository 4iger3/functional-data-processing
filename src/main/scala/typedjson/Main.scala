package typedjson

import scala.io.Source
import scala.util.Using
import scala.util.{Failure, Success}

object Main:
  def main(args: Array[String]): Unit =
    if args.length != 1 then
      Console.err.println("Usage: sbt \"run <path-to-json>\"")
      sys.exit(2)

    val content = Using(Source.fromFile(args(0), "UTF-8"))(_.mkString) match
      case Failure(error) =>
        Console.err.println(s"Could not read '${args(0)}': ${error.getMessage}")
        sys.exit(1)
      case Success(value) => value

    JsonData.parseTransactions(content) match
      case Left(errors) =>
        errors.foreach(error => Console.err.println(error.message))
        sys.exit(1)
      case Right(transactions) =>
        val totals = transactions
          .groupMapReduce(_.category)(_.amount)(_ + _)
          .toVector
          .sortBy(_._1)
        val grandTotal = transactions.map(_.amount).sum

        println(s"Processed ${transactions.size} transactions")
        println("Totals by category:")
        totals.foreach { case (category, amount) =>
          println(f"  $category%-16s ${amount.setScale(2, BigDecimal.RoundingMode.HALF_UP)}")
        }
        println(f"Grand total:      ${grandTotal.setScale(2, BigDecimal.RoundingMode.HALF_UP)}")
        println("Example calculated field (10% tax):")
        transactions.foreach { transaction =>
          val note = transaction.note.fold("")(value => s" - $value")
          println(
            f"  ${transaction.category}%-16s ${transaction.amountWithTax(BigDecimal("0.10"))}%s$note"
          )
        }
