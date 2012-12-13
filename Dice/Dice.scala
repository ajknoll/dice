import scala.util.parsing.combinator._

class Dice extends RegexParsers {
  sealed val natural : Parser[Int] = regex("""\d+""".r) ^^ {s => s.toInt}
  sealed def getOr1 (o : Option[Int]) : Int = {
    case i : Some[Int] => i.get
    case _ : None => 1
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
      ^^ { case ("+", y) => {x : Int => x + y}
           case ("-", y) => {x : Int => x - y}
           case ("*", y) => {x : Int => x * y}
           case ("/", y) => {x : Int => x / y}
         }
    )
}
