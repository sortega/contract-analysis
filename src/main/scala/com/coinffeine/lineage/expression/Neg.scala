package com.coinffeine.lineage.expression

case class Neg[N](x: Expression[N])(implicit num: Numeric[N]) extends Expression[N] {
  override def eval(bindings: Map[Symbol, N]): N = num.negate(x.eval(bindings))
  override def toString: String = s"-$x"
  override def ground = x.ground
  override def simplified = if (ground) Cons(eval(Map.empty)) else this
}
