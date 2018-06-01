object worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def sum(xs: List[Int]): Int = {
            def loop(acc: Int, xs: List[Int]): Int =
                    if (xs.isEmpty) acc
                    else loop(acc+xs.head, xs.tail)
            
            loop(0, xs)
    }                                             //> sum: (xs: List[Int])Int
    
    sum(List(2,3,4))                              //> res0: Int = 9


	     def max(xs: List[Int]): Int = {
      if (xs.isEmpty) throw new java.util.NoSuchElementException
      
      def loop(acc: Int, xs: List[Int]): Int =
        if (xs.isEmpty) acc
        else {
          if (xs.head>acc) loop(xs.head, xs.tail)
          else loop(acc, xs.tail)
        }
      loop(0, xs)
         
    }                                             //> max: (xs: List[Int])Int
    
    max(List(0,5,6,4,8,2,3,6,1,0))                //> res1: Int = 8
	


}