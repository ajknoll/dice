import scala.util.parsing.combinator._


class Dice extends RegexParsers {
  val natural : Parser[Int] = regex("""\d+""".r) ^^ {s => s.toInt}

  def roll : Parser[Int, Int] = ( opt(natural) ^^ {o => getOrElse(1)} 
                                ~ ("d" | "D") 
                                ~ natural
                                )
}
