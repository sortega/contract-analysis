package com.coinffeine.analysis.exchange

import com.coinffeine.analysis.{Payoff, Payoffs}
import com.coinffeine.lineage.Lineaged

case class Balances(btc: Payoffs, fiat: Payoffs) {
  private val num = implicitly[Numeric[Payoff]]
  import num._

  /** Computes the utility for the players given a consumer surplus and assuming linearity. */
  def utilities(consumerSurplus: Payoff): Payoffs = {
    val factor = consumerSurplus + Lineaged.value[BigDecimal](1)
    btc.scaleBy(bobFactor = factor) + fiat.scaleBy(samFactor = factor)
  }

  def pay(amount: Payoff): Balances = {
    val num = implicitly[Numeric[Payoff]]
    copy(fiat = fiat + Payoffs(bob = num.negate(amount), sam = amount))
  }

  def loseBtc(amounts: Payoffs): Balances = copy(btc = btc - amounts)
}
