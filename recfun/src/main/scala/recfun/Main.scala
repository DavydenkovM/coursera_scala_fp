// package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("balance")
    var str : String = "(I told him that it’s not (yet) done). (But he wasn’t listening)"
    println(balance(str.toList))
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
    def numOpens(chars: List[Char]): Int =
      _numOpens(chars, 0)

    def _numOpens(chars: List[Char], acc: Int): Int = {
      if (chars.isEmpty) 1
      else {
        val h = chars.head
        val t = chars.tail

        val n : Int =
          if (h == '(')
            acc + 1
          else if (h == ')')
            acc - 1
          else
            acc

        if (!t.isEmpty) _numOpens(t, n)
        else n
      }
    }

    val number = numOpens(chars)
    number == 0
  }

  /**
   * Exercise 3
    def countChange(money: Int, coins: List[Int]): Int = ???
   */
  }
