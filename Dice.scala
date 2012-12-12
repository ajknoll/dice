import scala.util.parsing.combinator._


class Dice extends RegexParsers {
  val natural : Parser[Int] = regex("""\d+""".r)

  def roll : Parser[Any] = opt(natural) ~ ("d" | "D") ~ natural
}
