package com.coinffeine.lineage.expression

case class Cons[N](value: N) extends Expression[N] {
  override def eval(bindings: Map[Symbol, N]): N = value
  override def toString = value.toString
  override val ground = true
  override val simplified = this
}
