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
  def pascal(c: Int, r: Int): Int = {
    if((c == 0) || (r == c)) 1
    else (pascal((c - 1), (r - 1)) + (pascal(c, (r - 1))))
  } 
    
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(chars: List[Char], parens: List[Char]): Boolean = 
      if(chars.isEmpty && parens.isEmpty)
        true
      else{
        if(chars.head == '(')
          check(chars.tail, '(':: parens)
        else
          if(parens.isEmpty)
            //#that means char is ) but there is no ( paren available in stack 
            false
          else
            check(chars.tail, parens.tail)
            }
    check(chars.filter(s => s == '(' || s == ')'), List())
          
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if((money < 0) || coins.isEmpty) 0
    else (countChange(money, coins.tail) + 
        countChange(money - coins.head, coins))
  // because: the number of ways to change amount a using n kind of coins equals:
  // the number of ways to change a using all but the first kind of coins, plus
  // the number of ways to change amount (a-d) using n coins, where d is the first denomination of coin
  }
}
