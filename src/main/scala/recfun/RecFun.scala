package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c <= 0 || r <= 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @scala.annotation.tailrec
    def loop(chars: List[Char], acc: List[Char]): Boolean = chars match {
      case hd :: tl =>
        if (hd == '(') {
          loop(tl, hd :: acc)
        } else if (hd == ')') {
          if (acc.isEmpty) false else loop(tl, acc.drop(1))
        } else {
          loop(tl, acc)
        }
      case _ =>
        if (acc.isEmpty) true else false
    }

    loop(chars, List())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money >0 && coins.nonEmpty){
      countChange(money,coins.tail) + countChange(money-coins.head,coins)
    }else{
      if(money == 0) 1 else 0
    }

  }
}
