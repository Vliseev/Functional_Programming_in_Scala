package recfun

import scala.collection.immutable.Stack

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
    println(countChange(301,List(5,10,20,50,100,200,500)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (r<c) 0
      else if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
  def balance_stack(chars: List[Char], stack: List[Char]): Boolean = chars match {
    case head :: Nil =>
      if (head == ')') stack match {
        case s_head :: Nil => if (s_head == '(') true else false
        case Nil => false
      }
      else if (stack.isEmpty) false else true
    case head :: tail => head match {
      case '(' => balance_stack(tail, head :: stack)
      case ')' => balance_stack(tail, if(stack.isEmpty) stack else stack.tail)
      case _ => balance_stack(tail,stack)
    }
  }


  def balance_counter(chars: List[Char], count:Int): Boolean = chars match {
    case head :: Nil =>
      if (head == ')') if (count == 1) true else false
      else if (count == 0) true else false

    case head :: tail => head match {
      case '(' => balance_counter(tail, count+1)
      case ')' => balance_counter(tail, count-1)
      case _ => balance_counter(tail,count)
    }
  }

  def balance(chars: List[Char]): Boolean = {
    balance_stack(chars,List[Char]())
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money==0) 1
      else if(coins.size==0 || money <0) 0
      else countChange(money-coins.last,coins) + countChange(money,coins.init)
    }
  }
