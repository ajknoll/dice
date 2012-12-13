import scala.util.parsing.combinator._

class Dice extends RegexParsers {
  val natural : Parser[Int] = regex("""\d+""".r) ^^ {s => s.toInt}
  def getOr1 (o : Option[Int]) : Int = o match {
    case Some(i) => i
    case None    => 1
  }

  def roll : Parser[(Int, Int)] =
    ( (opt(natural) ^^ {o : Option[Int] => getOr1(o)})
    ~ ("d" | "D")
    ~ natural
    //) ^^ {(count : Int, _ : String, sides : Int) => (count, sides)}
    ) ^^ {case (count: Int) ~ _ ~ (sides : Int) => (count, sides)}

  def repeats : Parser[Int] =
    opt(natural) ^^ {o : Option[Int] => getOr1(o)}

  def modifier : Parser[(Int => Int)] =
    (
      (("+" | "-" | "*" | "/") ~ natural)
      ^^ { case ("+" ~ y) => {x : Int => x + y}
           case ("-" ~ y) => {x : Int => x - y}
           case ("*" ~ y) => {x : Int => x * y}
           case ("/" ~ y) => {x : Int => x / y}
         }
    )
}

object ParseDice extends Dice {
  def main (args : Array[String]) {
    for (dice <- args) {
      val parsed = parseAll(roll, dice)
    }
  }
}
