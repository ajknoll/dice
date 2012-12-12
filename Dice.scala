import scala.util.parsing.combinator._
import scala.Option._


class Dice extends RegexParsers {
  val natural : Parser[Int] = regex("""\d+""".r) ^^ {s => s.toInt}

  def roll : Parser[(Int, Int)] =
    ( (opt(natural) ^^ {o : Option[Int] => o.getOrElse {_ : Int => 1}})
    ~ ("d" | "D") 
    ~ natural
    ) ^^ {(count : Int, _ : Any, sides : Int) => (count, sides)}
}
