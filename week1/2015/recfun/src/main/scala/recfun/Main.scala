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
  def pascal(c: Int, r: Int): Int = if (c == 0 || (c - r == 0)) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], balanced: Int): Boolean = if (chars.isEmpty) balanced == 0
    else if (chars.head == '(') balance(chars.tail, balanced + 1)
    else if (chars.head == ')' && balanced > 0) balance(chars.tail, balanced - 1)
    else if (chars.head == ')' && balanced <= 0) false
    else balance(chars.tail, balanced)
    balance(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeApp(money: Int, coins: List[Int]): Int =
      if (coins.isEmpty && money > 0) 0
      else if (money == 0) 1
      else if (money - coins.head < 0) 0
      else countChangeApp(money - coins.head, coins) + countChangeApp(money, coins.tail)
    countChangeApp(money, coins.sorted)
  }
}
