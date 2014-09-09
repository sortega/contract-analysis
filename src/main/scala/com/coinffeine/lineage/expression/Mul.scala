package com.coinffeine.lineage.expression

case class Mul[N](x: Expression[N], y: Expression[N])(implicit num: Numeric[N]) extends BinOp[N] {
  override protected def op: (N, N) => N = num.times
  override protected def symbol: String = "*"
}

