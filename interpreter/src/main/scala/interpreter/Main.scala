package interpreter

import Lisp._

object Main extends App {
  import java.io.{BufferedReader, InputStreamReader}
  val in = new BufferedReader(new InputStreamReader(System.in))
  
  def repl {
    print("lisp> ")
    val input = in.readLine()
    println(Lisp.lisp2string(Lisp.evaluate(input)))
    repl
  }
  
  repl
  
}

object LispCode {
  // TODO: implement the function `reverse` in Lisp.
  // From a list (a, b, c, d) it should compute (d, c, b, a)
  // Write it as a String, and test it in your REPL
  val reverse = """
  def (reverse L acc) (
    if (null? L)
      acc
        (reverse (cdr L) (cons (car L) acc)))
  """
 
  // TODO: implement the function `differences` in Lisp.
  // From a list (a, b, c, d ...) it should compute (a, b-a, c-b, d-c ...)
  // You might find useful to define an inner loop def
  val differences = """
  def (differences L) (
    def (inner x y acc) (
      if (null? x)
        (reverse acc nil)
        (inner (cdr x) (cdr y) (cons (- (car x) (car y)) acc))
    )
    (inner L (cons 0 L) nil)
  )
  """
  val rebuildList = """
  def (rebuildList L) (
    def (inner x count acc) (
      if (null? x)
        (reverse acc nil)
        (inner (cdr x) (+ count (car x)) (cons (+ count (car x)) acc))
    )
  (inner L 0 nil)
  )
  """
  
  val withDifferences: String => String =
    (code: String) => "(" + reverse + " (" + differences + " (" + rebuildList + " " + code + ")))"
}
