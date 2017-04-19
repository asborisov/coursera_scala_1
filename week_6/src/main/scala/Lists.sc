import math.Ordering

// Lecture 5.1
def flatten(xs: List[Any]): List[Any] = {
  def flattenElement(x: Any): List[Any] = x match {
    case y :: ys => flattenElement(y) ++ flatten(ys)
    case y => List(y)
  }

  xs match {
    case List() => Nil
    case y :: ys => flattenElement(y) ++ flatten(ys)
    case x => x
  }
}
flatten(List(List(1, 1), 2, List(3, List(5, 8))))

// Lecture 5.2
def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (x, Nil) => x
    case (Nil, y) => y
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge(xs1, ys)
      else y :: merge(xs, ys1)
  }
merge(List(1, 2), List(3, 4))

object mergesort {
  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] =
        (xs, ys) match {
          case (x, Nil) => x
          case (Nil, y) => y
          case (x :: xs1, y :: ys1) =>
            if (ord.lt(x, y)) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }

      val (fst, scn) = xs splitAt n
      merge(msort(fst), msort(scn))
    }
  }
}

val nums = List(5, 4, 0, 2, -3, 1)
mergesort.msort(nums)


def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => y*y :: squareList(ys)
}

def squareListMap(xs: List[Int]): List[Int] = xs map (x=>x*x)

squareList(List(1, 2, 3, 4))
squareListMap(List(1, 2, 3, 4))

