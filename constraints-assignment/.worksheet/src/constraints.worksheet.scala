package constraints
import constraints._
import cafesat.api.FormulaBuilder._
import cafesat.api.Formulas._
import cafesat.api.Solver._

import Arithmetic._

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(219); 
  println("Welcome to the Scala worksheet");$skip(247); 
  


	
	def lessEquals(n1: List[Formula], n2: List[Formula]): Formula = {
    val (x, y) = sameLength(n1, n2)
    val temp = x.zip(y).dropWhile{t => t._1 == t._2}
    if (temp.isEmpty) true
    else if (temp.head._1==True) false
    else true
  };System.out.println("""lessEquals: (n1: List[cafesat.api.Formulas.Formula], n2: List[cafesat.api.Formulas.Formula])cafesat.api.Formulas.Formula""");$skip(37); 
  
		val nd1: List[Formula] = List();System.out.println("""nd1  : List[cafesat.api.Formulas.Formula] = """ + $show(nd1 ));$skip(36); 
    val nd2: List[Formula] = List();System.out.println("""nd2  : List[cafesat.api.Formulas.Formula] = """ + $show(nd2 ));$skip(34); 
    val rd = lessEquals(nd1, nd2);System.out.println("""rd  : cafesat.api.Formulas.Formula = """ + $show(rd ))}
  
  
  
  
  
  
}
