package com.coinffeine.lineage.expression

trait Expression[N] {

  def eval(bindings: Map[Symbol, N]): N

  /** Best effort simplification of this expression */
  def simplified: Expression[N]

  def ground: Boolean
}
