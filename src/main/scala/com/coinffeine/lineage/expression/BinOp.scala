package com.coinffeine.lineage.expression

trait BinOp[N] extends Expression[N] {
  val x: Expression[N]
  val y: Expression[N]

  protected def op: (N, N) => N
  protected def symbol: String
  protected def arithmeticSimplification: Expression[N] = this

  final override def eval(bindings: Map[Symbol, N]): N = op(x.eval(bindings), y.eval(bindings))
  final override def toString = s"($x $symbol $y)"
  final override def ground = x.ground && y.ground
  final override def simplified =
    if (ground) Cons(eval(Map.empty))
    else arithmeticSimplification
}
