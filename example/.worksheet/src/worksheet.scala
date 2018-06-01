object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  println("Welcome to the Scala worksheet");$skip(225); 
  
  def sum(xs: List[Int]): Int = {
            def loop(acc: Int, xs: List[Int]): Int =
                    if (xs.isEmpty) acc
                    else loop(acc+xs.head, xs.tail)
            
            loop(0, xs)
    };System.out.println("""sum: (xs: List[Int])Int""");$skip(26); val res$0 = 
    
    sum(List(2,3,4));System.out.println("""res0: Int = """ + $show(res$0));$skip(330); 


	     def max(xs: List[Int]): Int = {
      if (xs.isEmpty) throw new java.util.NoSuchElementException
      
      def loop(acc: Int, xs: List[Int]): Int =
        if (xs.isEmpty) acc
        else {
          if (xs.head>acc) loop(xs.head, xs.tail)
          else loop(acc, xs.tail)
        }
      loop(0, xs)
         
    };System.out.println("""max: (xs: List[Int])Int""");$skip(40); val res$1 = 
    
    max(List(0,5,6,4,8,2,3,6,1,0));System.out.println("""res1: Int = """ + $show(res$1))}
	


}
