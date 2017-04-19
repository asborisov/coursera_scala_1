object polynomials {

  //  class Poly(val terms: Map[Int, Double]) {
  class Poly(terms0: Map[Int, Double]) {
    def this(binding: (Int, Double)*) = this(binding.toMap)

    val terms: Map[Int, Double] = terms0.withDefaultValue(0.0)

    //    def +(other: Poly) = new Poly(terms ++ other.terms.map(adjust))

    def +(other: Poly) = new Poly(other.terms.foldLeft(terms)(addTerm))

    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
//      terms.updated(term._1, term._2 + terms(term._1))

//      val (exp, coeff) = term
//      terms + (exp -> (coeff + terms(exp)))
      
      terms + (term._1 -> (term._2 + terms(term._1)))
    }

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      //      if (terms.contains(exp)) (exp, coeff + terms(exp))
      //      else term
      //
      //      terms.get(exp) match {
      //        case None => term
      //        case Some(coeff2) => (exp, coeff + coeff2)
      //      }
      (exp, coeff + terms(exp))
    }

    override def toString: String =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp)
        .mkString(" + ")

    //    override def toString: String = terms
    //      .toList.sorted.reverse
    //      .map(e => e._2 + "x^" + e._1)
    //      .mkString(" + ")
  }

  val p1m = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
  val p2m = new Poly(Map(0 -> 3.0, 3 -> 7.0))
  val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
  val p2 = new Poly(0 -> 3.0, 3 -> 7.0)

  p1m + p2m
  p1 + p2
}