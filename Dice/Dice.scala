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
  def padLeft (s : String) (to : Int) : String = s.reverse.padTo(i, ' ').reverse

  def main (args : Array[String]) {
    for ((a, i) <- args.view.zipWithIndex) {
      parseAll(diceRoll, a) match {
        case Success(r, _) => {
          val (repeats, count, sides, mods) = (r._1._1, r._1._2._1, r._1._2._2, r._2)
          for (j <- 0 until repeats) {
            val (outcome, rolls) = evaluate (count) (sides) (mods)
            val repeatLength = repeats.toString.length
            val outcomeLength = (sides * count).toString.length
            val rollLength = sides.toString.length
            print(padLeft (i.toString. ++ "." ++ j.toString ++ ": ") 
                          (indexLength + repeatLength + 3))
            print(padLeft (outcome.toString) (outcomeLength))
            print(" [")
            for ((r, i) <- rolls.view.zipWithIndex) {
              print(padLeft (r.toString) (rollLength))
            }
            print("] (" ++ a ++ ")")
          }
        }
        case _ => println("parser failure")
      }
    }
  }
}
