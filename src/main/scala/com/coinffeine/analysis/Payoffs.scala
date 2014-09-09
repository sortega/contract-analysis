package com.coinffeine.analysis

import com.coinffeine.lineage.Lineaged

case class Payoffs(bob: Payoff, sam: Payoff) extends Seq[Payoff] {
  private val num = implicitly[Numeric[Payoff]]
  import num._

  def apply(player: Player) = player match {
    case Bob => bob
    case Sam => sam
  }

  def +(rhs: Payoffs) = Payoffs(
    bob + rhs.bob,
    sam + rhs.sam
  )

  def -(rhs: Payoffs) = Payoffs(
    bob - rhs.bob,
    sam - rhs.sam)

  def *(rhs: Payoffs) = Payoffs(
    bob * rhs.bob,
    sam * rhs.sam
  )

  def scaleBy(bobFactor: BigDecimal, samFactor: BigDecimal): Payoffs =
    scaleBy(Lineaged.value(bobFactor), Lineaged.value(samFactor))

  def scaleBy(bobFactor: Payoff = Lineaged.value(1),
              samFactor: Payoff = Lineaged.value(1)): Payoffs =
    Payoffs(bob * bobFactor, sam * samFactor)

  def switch = Payoffs(bob = sam, sam = bob)

  private val seq_ = Seq(bob, sam)

  override def apply(idx: Int) = seq_(idx)

  override def iterator = seq_.iterator

  override def length = 2
}

object Payoffs {

  val zero = Payoffs.fill(Lineaged.value[BigDecimal](0))

  def fill(payoff: Payoff) = Payoffs(sam = payoff, bob = payoff)

  def apply(pairs: (Player, Payoff)*): Payoffs = {
    require(pairs.size == 2)
    val map = pairs.toMap
    require(map.size == 2)
    Payoffs(bob = map(Bob), sam = map(Sam))
  }
}
