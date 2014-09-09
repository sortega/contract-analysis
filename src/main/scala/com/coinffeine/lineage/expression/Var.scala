package com.coinffeine.lineage.expression

case class Var[N](name: Symbol) extends Expression[N] {
  override def eval(bindings: Map[Symbol, N]): N = bindings(name)
  override def toString = name.name
  override val ground = false
  override val simplified = this
}
