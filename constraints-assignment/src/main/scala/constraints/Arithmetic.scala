package constraints

import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import scala.annotation.tailrec
import scala.math

/**
  * This object contains utility functions for encoding
  * some arithmetic constraints as boolean ones
  */
object Arithmetic {

  /**
   * Transforms a positive integer in binary form into its integer representation.
   * The `head` element of the input list contains the most
   * significant bit (the list is in big-endian form).
   */
  def binary2int(n: List[Boolean]): Int = {
    def temp(x : List[Boolean], acc : Int): Int = {
      if (x.isEmpty) acc 
      else if (x.head) temp(x.tail, acc+(1 << (x.length-1)))
      else temp(x.tail, acc)
    }
    temp(n, 0)
  }

  /**
   * Encodes a positive integer number into base 2.
   * The `head` element of the resulting list contains the most significant
   * bit. This function should not return unnecessary leading zeros.
   */
  def int2binary(n: Int): List[Boolean] = {
    def temp(x : Int, acc : List[Boolean] ) : List[Boolean] = {
      if (x == 0) acc
      else if (x%2 == 0) temp(x/2, false :: acc)
      else temp((x-1)/2, true :: acc)
    }
    
    if (n==0) List(false)
    else temp(n, List())
  }


  /**
   * This function takes two arguments, both representing positive
   * integers encoded in binary as lists of propositional formulas
   * (true for 1, false for 0). It returns
   * a formula that represents a boolean circuit that constraints
   * `n1` to be less than or equal to `n2`
   */
//  digital comparator
  def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {    
    
    val (x, y) = sameLength(n1, n2)
    val z = x.zip(y).map{ case (a, b)=> (a && b) || (!a && !b)}
    
    val equals: Formula = z.foldLeft[Formula](true)(_ && _)
    val less : Formula = x.zip(y).zipWithIndex.map{ case ((a, b), n) => (!a && b && z.take(n).foldLeft[Formula](true)(_ && _))}.foldLeft[Formula](false)(_ || _)
    less || equals  
  }


  /**
   * A full adder is a circuit that takes 3 one bit numbers, and returns the
   * result encoded over two bits: (cOut, s)
   */
  def fullAdder(a: Formula, b: Formula, cIn: Formula): (Formula, Formula) = {
   (or(and(a, b), and(cIn, a xor b)), a xor b xor cIn)

  }

  /**
   * This function takes two arguments, both representing positive integers
   * encoded as lists of propositional variables. It returns a pair.
   *
   * The first element of the pair is a `List[Formula]`, and it represents
   * the resulting binary number.
   * The second element is a set of intermediate constraints that are created
   * along the way.
   *
   */
  def adder(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
    
//    def temp (accList : List[Formula], accSet : Set[Formula], n1 : List[Formula], n2: List[Formula], cIn : Formula) : (List[Formula], Set[Formula]) = {
//      val (s, cout) = (propVar(), propVar())
//      (n1, n2) match {
//        case (Nil, Nil) => (cout :: accList, accSet ++ Set(cout iff cIn))
//        case (a :: as, b :: bs) => {
//          val add = fullAdder(a, b, cIn)
//          temp (s::accList, accSet ++ Set(s iff add._2, cout iff add._1), as, bs, cout)
//        }
//        case _ => sys.error("unexpected case")
//      }
//      
//    }
//    
//      val (x, y) = sameLength(n1, n2)
//      temp(Nil, Set(), x.reverse, y.reverse, false)
//  }
    
    def temp(n1: List[Formula], n2: List[Formula]): (List[Formula], Set[Formula]) = {
      val (s, cout) = (propVar(), propVar())
      (n1, n2) match {
        
        case (x :: Nil, y :: Nil) => {
          val (head, last) = fullAdder(x, y, False)
          (s :: cout :: Nil, Set(s iff head, cout iff last))
        }
        case (x :: xs, y :: ys) => {
          val (z :: zs, constraint) = temp(xs, ys)
          val (head, second) = fullAdder(x, y, z)
          (s :: cout :: zs, constraint + (s iff head, cout iff second))
        }
        case _ => sys.error("Unexpected case")
      }

    }
    val (x, y) = sameLength(n1, n2)
    temp(x, y)
    
  }
  

  /**
   * A helper function that creates a less-equals formula
   * taking an integer and a formula as parameters
   */
  def lessEqualsConst(cst: Int, n: List[Formula]): Formula = {
    lessEquals(int2binary(cst), n)
  }

  /**
   * A helper function that creates a less-equals formula
   * taking a formula and an integer as parameters
   */
  def lessEqualsConst(n: List[Formula], cst: Int): Formula = {
    lessEquals(n, int2binary(cst))
  }

  
  /**
   * method to have the same length for two list
   */
  def sameLength(n1: List[Formula], n2: List[Formula]): (List[Formula], List[Formula]) = {
      def temp(size: Int, acc: List[Formula]): List[Formula] = {
        if (size <= 0) acc
        else temp(size - 1, false :: acc)
      }
      
      (n1.size - n2.size) match {
        case 0 => (n1, n2)
        case x => if (x < 0) (temp(-x, n1), n2) else (n1, temp(x, n2))
      }
    }

}
