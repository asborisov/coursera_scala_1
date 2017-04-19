// Lecture 5.4
val data = List("a", "a", "a", "b", "c", "c", "a")

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val span: (List[T], List[T]) = xs.span(i => i == x)
    span._1 :: pack[T](span._2)
}

def encode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val span = xs.span(i => i == x)
    (x, span._1.length) :: encode(span._2)
}

def encodeMap[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (x => (x.head, x.length))

pack(data)
encode(data)
encodeMap(data)