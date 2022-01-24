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

  def main(args: Array[String]): Unit =
    println(fib(6))
}
