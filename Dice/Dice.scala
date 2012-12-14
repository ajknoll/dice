import scala.util.parsing.combinator._
import scala.util.Random

class Dice extends RegexParsers {
  val natural : Parser[Int] = regex("""\d+""".r) ^^ {s => s.toInt}
  def getOr1 (o : Option[Int]) : Int = o match {
      case Some(i) => i
      case None    => 1
    }

  def diceRoll : Parser[Int ~ (Int, Int) ~ List[Int => Int]] =
    ( ((repeats?) ^^ {o => getOr1(o)})
    ~ roll
    ~ (modifier*)
    )

  def roll : Parser[(Int, Int)] =
    ( ((natural?) ^^ {o => getOr1(o)})
    ~ ("d" | "D")
    ~ natural
    ) ^^ {case (count: Int) ~ _ ~ (sides : Int) => (count, sides)}

  def repeats: Parser[Int] =
    natural <~ "r"

  def modifier : Parser[(Int => Int)] =
    ( (("+" | "-" | "*" | "/") ~ natural)
      ^^ { case ("+" ~ y) => {x : Int => x + y}
           case ("-" ~ y) => {x : Int => x - y}
           case ("*" ~ y) => {x : Int => x * y}
           case ("/" ~ y) => {x : Int => x / y}
         }
    )

  def evaluate (count : Int) (sides : Int) (mods : List[Int => Int]) : (Int, List[Int]) =
    { val rand = Random
      val results = for (_ <- 1 to count) yield (rand.nextInt(sides) + 1)
      ((results.sum /: mods) ((acc, m) => m(acc)), results.toList)
    }
}

object ParseDice extends Dice {
  def main (args : Array[String]) {
    for (a <- args; i <- Iterator.range(1, args.length)) {
      parseAll(diceRoll, a) match {
        case Success(r, _) => {
          val repeats = r._1._1
          for (j <- Iterator.range(0, repeats)) {
            val (outcome, rolls) = evaluate (r._1._2._1) (r._1._2._2) (r._2)
            println(i.toString ++ ": "
                 ++ outcome.toString
                 ++ " [" ++ rolls.toString 
                 ++ "] (" ++ a ++ ")")
          }
        }
        case _ => println("parser failure")
      }
    }
  }
}
