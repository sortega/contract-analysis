package com.coinffeine.lineage.expression

case class Sub[N](x: Expression[N], y: Expression[N])(implicit num: Numeric[N]) extends BinOp[N] {
  override protected def op: (N, N) => N = num.minus
  override protected def symbol: String = "-"
}
