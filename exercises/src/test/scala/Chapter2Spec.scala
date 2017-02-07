import org.scalatest.{FlatSpec, Matchers}

class Chapter2Spec extends FlatSpec with Matchers {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = A => {
    val f1: B => C = f(A, _)
    f1
  }

  def curry2[A, B, C](f: (A, B) => C): A => (B => C) = A => B => f(A, B)
  def curry3[A, B, C](f: (A, B) => C): A => (B => C) = (a:A) => (b:B) => f(a, b)
  def curry4[A, B, C](f: (A, B) => C): A => (B => C) = (a) => (b) => f(a, b)
  def curry5[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (A, B) => f(A)(B)

  def compose[A,B,C](f: B=> C, g: A=> B): A=> C = (A) => f(g(A))


  def cArg(letterA: Int, letterB: String): Double = 3.2

  it should "curry" in {
    val thing: (Int) => (String) => Double = curry[Int, String, Double](cArg(_, _))

    val resultAsDouble: Double = thing(8)("string")
    resultAsDouble shouldBe (3.2)
  }

  it should "curry for Jim" in {
    val partialFunction1: (Int => (String => Double)) = curry[Int, String, Double]((a:Int, b:String) => 3.2)

    val partialFunction2: (String) => Double = partialFunction1(8)
    val resultAsDouble: Double = partialFunction2("string")
    resultAsDouble shouldBe (3.2)
  }

  it should "also work with curry2" in {
    val thing = curry2[Int, String, Double](cArg(_, _))
    val resultAsDouble: Double = thing(8)("string")
    resultAsDouble shouldBe (3.2)
  }

  it should "represent Set with function" in {
    type TimSet = Int => Boolean

    def contains(s: TimSet, item: Int):Boolean = s(item)

    def singleSet(item: Int): TimSet = { (x: Int) => x == item}

//    def exists(s: TimSet, ):Boolean = {
//      val upperBound = 1000
//      def iter(i:Int):Boolean ={
//        if (i > 1000) false
//        else if(contains(s, item)) true
//        else iter(i + 1)
//      }
//      iter(-1000)
//    }


    val s1 = singleSet(1) //Set(1)a
    val s2 = singleSet(2)

//    assert(exists(s1, 1))
//    assert(!exists(s1, 2))

    assert(contains(s1, 1))
    assert(!contains(s1,2))

    assert(contains(s2, 2))
    assert(!contains(s2,1))

  }
}
