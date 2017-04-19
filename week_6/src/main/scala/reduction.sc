def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

def lengthFun[T](xs: List[T]): Int =
  xs.foldRight(0)((_, acc) => acc + 1 )

lengthFun(List(0, 1, 2, 3, 4, 5))
mapFun[Int, Int](List(1, 2, 3), x => x + 1)