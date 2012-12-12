import scala.util.parsing.combinator._

val natural : Parser[Int] = regex("\d+")

class Dice extends RegexParsers {
  def roll : Parser[Any] = opt(natural) ~ ("d" | "D") ~ natural
}
