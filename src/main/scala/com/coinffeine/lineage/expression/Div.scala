package com.coinffeine.lineage.expression

case class Div[N](x: Expression[N], y: Expression[N])(implicit num: Fractional[N]) extends BinOp[N] {
  override protected def op: (N, N) => N = num.div
  override protected def symbol: String = "/"
}
