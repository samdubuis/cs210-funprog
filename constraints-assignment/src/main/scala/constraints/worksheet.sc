package constraints
import constraints._
import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import Arithmetic._

object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  


	
	def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    val (x, y) = sameLength(n1, n2)
    val temp = x.zip(y).dropWhile{t => t._1 == t._2}
    if (temp.isEmpty) true
    else if (temp.head._1==True) false
    else true
  }                                               //> lessEquals: (n1: List[cafesat.api.Formulas.Formula], n2: List[cafesat.api.Fo
                                                  //| rmulas.Formula])cafesat.api.Formulas.Formula
  
		val nd1: List[Formula] = List()   //> nd1  : List[cafesat.api.Formulas.Formula] = List()
    val nd2: List[Formula] = List()               //> nd2  : List[cafesat.api.Formulas.Formula] = List()
    val rd = lessEquals(nd1, nd2)                 //> rd  : cafesat.api.Formulas.Formula = âŠ¤
  
  
  
  
  
  
}