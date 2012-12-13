import scala.util.parsing.combinator._

class Dice extends RegexParsers {
  val natural : Parser[Int] = regex("""\d+""".r) ^^ {s => s.toInt}

  def roll : Parser[(Int, Int)] =
    ( (opt(natural) ^^ {o : Option[Int] => o.getOrElse {_ : Int => 1}})
    ~ ("d" | "D") 
    ~ natural
    ) ^^ {(count : Int, _ : String, sides : Int) => (count, sides)}

  def modifier : Parser[(Int => Int)] =
    (
      (("+" | "-" | "*" | "/") ~ natural)
      ^^ { case ("+", y) => {x : Int => x + y}
         }
    )
}
