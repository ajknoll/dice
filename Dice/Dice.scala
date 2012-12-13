import scala.util.parsing.combinator._
import scala.util.Random

class Dice extends RegexParsers {
  val natural : Parser[Int] = regex("""\d+""".r) ^^ {s => s.toInt}
  def getOr1 (o : Option[Int]) : Int = o match {
      case Some(i) => i
      case None    => 1
    }

  def diceRoll : Parser[(Int, Int) ~ List[Int => Int]] =
    ( roll
    ~ (modifier*)
    )

  def roll : Parser[(Int, Int)] =
    ( (opt(natural) ^^ {o : Option[Int] => getOr1(o)})
    ~ ("d" | "D")
    ~ natural
    ) ^^ {case (count: Int) ~ _ ~ (sides : Int) => (count, sides)}

  def repeats: Parser[Int] =
    opt(natural) ^^ {o : Option[Int] => getOr1(o)}

  def modifier : Parser[(Int => Int)] =
    ( (("+" | "-" | "*" | "/") ~ natural)
      ^^ { case ("+" ~ y) => {x : Int => x + y}
           case ("-" ~ y) => {x : Int => x - y}
           case ("*" ~ y) => {x : Int => x * y}
           case ("/" ~ y) => {x : Int => x / y}
         }
    )

  def evaluate (dice : (Int, Int) ~ List[Int => Int]) : Int =
    { val rand = Random
      dice match { case (count, sides) ~ mods =>
        val results = for (_ <- 1 to count) yield (rand.nextInt(sides) + 1)
        (results.sum /: mods) ((acc, m) => m(acc))
      }
    }
}

object ParseDice extends Dice {
  def main (args : Array[String]) {
    for (a <- args) {
      parseAll(diceRoll, a) match {
        case Success(r, _) => println(evaluate(r).toString ++ " [" ++ a ++ "]")
        case _ => println("parser failure")
      }
    }
  }
}
