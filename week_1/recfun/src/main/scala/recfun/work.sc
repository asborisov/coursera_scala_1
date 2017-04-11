import scala.annotation.tailrec
def factorial(n: Int): Long = {
  @tailrec
  def loop(acc: Long, n: Long): Long =
    if (n == 0) acc
    else loop(acc * n, n - 1)
  loop(1, n)
}
//def pascal(c: Int, r: Int): Int = {
//
//
//  if (c < 0 || r < 0) 0
//  else if (r < 2) 1
//  else factorial(r) / factorial(c) * factorial(r - c)
//}

def pascal(c: Int, r: Int): Int = {
  def loop(col: Int, row: Int): Int = {
    if (col <= 0 || col >= row) 1
    else if (col == 1 || col == row) row
    else if (row <= 1) 1
    else loop(col - 1, row - 1) + loop(col, row - 1)
  }
  loop(c, r)
}


//pascal(100,200)
pascal(1,3) == 3
pascal(0,2) == 1
