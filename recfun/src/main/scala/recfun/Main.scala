package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == c) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def count(chars: List[Char], left: Int): Boolean  = {
      if (chars.isEmpty) left == 0 
      else if (left < 0) false
      else if (chars.head == '(') count(chars.tail, left + 1)
      else if (chars.head == ')') count(chars.tail, left - 1)
      else count(chars.tail, left)
      
    }
    count(chars, 0)  
    
  }
  /**
   * Exercise 3
   */
  
  def countChange(money: Int, coins: List[Int]): Int = {
    def cc(amount: Int, step: Int): Int = {
    	if (amount == 0) 1
    	else if (amount < 0 || step == -1) 0
    	else cc(amount, step - 1) + cc(amount - coins(step), step)
    }
    cc(money, coins.size - 1)
  }
}
