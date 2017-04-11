import java.util.NoSuchElementException

import scala.annotation.tailrec

trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}
class Nil[T] extends List[T] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

@tailrec
def nth[T](n: Int, list: List[T]): T =
  if (n == 0) list.head
  else if (list.tail.isEmpty) throw new IndexOutOfBoundsException()
  else nth(n - 1, list.tail)

val list = new Cons(1, new Cons(2, new Cons(3, new Nil[Int])))
nth(1, list)
nth(5, list)