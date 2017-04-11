trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepand[U >: T](elem: U): List[U] = new Cons(elem, this)
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
}
object Nil extends List[Nothing] {
  def isEmpty = true
  def head: Nothing = throw new NoSuchElementException
  def tail: Nothing = throw new NoSuchElementException
}

object List {
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))

  def apply[T](x1: T): List[T] = new Cons[T](x1, Nil)
  def apply[T](): List[T] = Nil
}