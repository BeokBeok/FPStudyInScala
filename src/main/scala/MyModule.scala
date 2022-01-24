import scala.annotation.tailrec

object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n else n

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)
    }

    go(n, 0, 1)
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length) true
      else if (ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  def main(args: Array[String]): Unit =
    println(formatResult("The absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
}
