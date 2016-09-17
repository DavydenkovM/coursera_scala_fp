package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def process(chars: List[Char], acc: List[Char]): Boolean =
      if (chars.isEmpty) acc.isEmpty
      else {
        chars.head match {
          case '(' => process(chars.tail, acc :+ chars.head)
          case ')' =>  if (acc.contains('('))
                         process(chars.tail, acc.dropRight(1))
                       else
                         false
          case _ => process(chars.tail, acc)
        }
      }

    val list = List[Char]()
    process(chars, list)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match {
      case 0 => 1
      case x if x < 0 => 0
      case x if x >= 1 && coins.isEmpty => 0
      case _ => countChange(money, coins.tail) +
                countChange(money - coins.head, coins)
    }
  }
}
