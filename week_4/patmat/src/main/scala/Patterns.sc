trait Exp {
  def show(): String = this match {
    case Number(n) => n.toString
    case Sum(e1, e2) => e1.show + " + " + e2.show
  }
}
case class Number(n: Int) extends Exp
case class Sum(e1: Exp, e2: Exp) extends Exp

Sum(Number(1), Number(2)).show