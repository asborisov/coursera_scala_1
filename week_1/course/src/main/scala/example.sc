import scala.annotation.tailrec

def factorial(n: Int) : Int = {
  @tailrec
  def InnerFactorial(value: Int, n: Int): Int =
    if (n == 0) value else InnerFactorial(value * n, n - 1)

  InnerFactorial(1, n)
}

factorial(4)