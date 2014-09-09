package com.coinffeine.lineage.expression

case class Plus[N](x: Expression[N], y: Expression[N])(implicit num: Numeric[N]) extends BinOp[N] {
  override protected def op: (N, N) => N = num.plus
  override protected def symbol: String = "+"
}
