package com.coinffeine.lineage

import com.coinffeine.lineage.expression._

case class Lineaged[N](expression: Expression[N], bindings: Map[Symbol, N]) {
  def value: N = expression.eval(bindings)
  def simplified: Lineaged[N] = copy(expression = expression.simplified)
}

object Lineaged {

  def variable[N: Numeric](name: Symbol, value: N): Lineaged[N] =
    Lineaged(Var(name), Map(name -> value))

  def value[N: Numeric](value: N): Lineaged[N] = Lineaged(Cons(value), Map.empty)

  implicit def lineagedAsFractional[N: Fractional] = new Fractional[Lineaged[N]]() {
    private val num = implicitly[Fractional[N]]

    override def plus(x: Lineaged[N], y: Lineaged[N]) =
      Lineaged(Plus(x.expression, y.expression), combine(x.bindings, y.bindings))
    override def minus(x: Lineaged[N], y: Lineaged[N]) =
      Lineaged(Sub(x.expression, y.expression), combine(x.bindings, y.bindings))
    override def times(x: Lineaged[N], y: Lineaged[N]) =
      Lineaged(Mul(x.expression, y.expression), combine(x.bindings, y.bindings))
    override def div(x: Lineaged[N], y: Lineaged[N]) =
      Lineaged(Div(x.expression, y.expression), combine(x.bindings, y.bindings))
    override def negate(x: Lineaged[N]) = Lineaged(Neg(x.expression), x.bindings)

    override def toDouble(x: Lineaged[N]) = num.toDouble(x.value)
    override def toFloat(x: Lineaged[N]) = num.toFloat(x.value)
    override def toInt(x: Lineaged[N]) = num.toInt(x.value)
    override def toLong(x: Lineaged[N]) = num.toLong(x.value)
    override def fromInt(x: Int): Lineaged[N] = Lineaged.value(num.fromInt(x))

    override def compare(x: Lineaged[N], y: Lineaged[N]) = num.compare(x.value, y.value)

    private def combine(leftBindings: Map[Symbol, N], rightBindings: Map[Symbol, N]): Map[Symbol, N] = {
      for(commonKey <- leftBindings.keySet.intersect(rightBindings.keySet)) {
        require(leftBindings(commonKey) == rightBindings(commonKey),
          s"Incompatible bindings on $commonKey: $leftBindings vs $rightBindings")
      }
      leftBindings ++ rightBindings
    }
  }

  implicit def convertValues[N: Numeric](value: N): Lineaged[N] = Lineaged.value(value)
}
