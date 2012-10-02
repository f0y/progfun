import scala.annotation.tailrec

object test1 {

	def generalized(operator: (Int, Int) => Int, init: Int)(f: Int => Int)(a: Int, b: Int): Int = {
		@tailrec
		def loop(a:Int, acc:Int): Int = {
			if (a > b) acc
			else loop(a + 1, operator(acc, f(a)))
		}
		return loop(a, init)
	
	}                                         //> generalized: (operator: (Int, Int) => Int, init: Int)(f: Int => Int)(a: Int,
                                                  //|  b: Int)Int

	def sum = generalized((a: Int, b: Int) => a + b, 0)(_)
                                                  //> sum: => (Int => Int) => ((Int, Int) => Int)
	def product = generalized((a: Int, b: Int) => a * b, 1)(_)
                                                  //> product: => (Int => Int) => ((Int, Int) => Int)
	
	def fact(n: Int): Int = {
		product(x => x)(1, n)
	}                                         //> fact: (n: Int)Int
	
	fact(4)                                   //> res0: Int = 24
	product(x => x)(1,3)                      //> res1: Int = 6
}