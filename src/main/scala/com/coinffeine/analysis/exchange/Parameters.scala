package com.coinffeine.analysis.exchange

import com.coinffeine.analysis.{Payoff, Payoffs}
import com.coinffeine.lineage.Lineaged

/** Parameters of an exchange game.
  *
  * @constructor
  * @param steps             The number of steps in which the exchange will happen
  * @param contractAmount    The principal we want to exchange
  * @param consumerSurplus   The percentage of how much each player values the currency they don't
  *                          own. For example, a value of 0.1 means that Bob values 1 BTC like 1.1
  *                          fiat units and Sam values 1 fiat unit like 1.1 BTC
  */
case class Parameters(
  steps: Int = 10,
  contractAmount: Payoff = Lineaged.variable('contractAmount, 100),
  consumerSurplus: Payoff = Lineaged.variable('consumerSurplus, 0)) {

  private val num = implicitly[Fractional[Payoff]]
  import num._

  require(steps > 0)
  require(contractAmount.value > 0)

  val noPayoff = Lineaged.value[BigDecimal](0)

  /** The amount of value that will be exchanged on each step */
  val contractStep: Payoff = contractAmount / Lineaged.value[BigDecimal](steps)

  /** The amount of fiat each player needs in order to enter the exchange. We assume
    * 1 fiat unit = 1 BTC, since the specific exchange rate doesn't affect the mechanics
    * of the game.
    */
  val fiatAmounts: Payoffs = Payoffs(bob = contractAmount, sam = noPayoff)

  /** The deposit amount each players needs for the exchange */
  val depositAmounts: Payoffs = Payoffs(
    bob = contractStep * Lineaged.value(2),
    sam = contractStep)

  /** The amount of bitcoins that will be committed into the micropayment channel */
  val btcAmounts: Payoffs = depositAmounts + Payoffs(
    bob = noPayoff,
    sam = contractAmount
  )

  /** The total amount of value each player needs in order to enter the exchange */
  val initialBalances: Balances = Balances(btcAmounts, fiatAmounts)
  val initialValues: Payoffs = initialBalances.utilities(consumerSurplus)

  val refundPenalization: Payoffs = Payoffs(contractStep, contractStep)

  val firstOffer = Payoffs(bob = noPayoff, sam = contractAmount)
  val exchangeDelta = Payoffs(
    bob = contractStep,
    sam = -contractStep

  )
  /** The special, final offer Bob will offer Sam which includes getting back the deposits */
  val finalOffer = btcAmounts.switch + depositAmounts

  /** The sequence of micro payment channel transactions */
  val offers: Vector[Payoffs] =
    Vector.fill(steps - 1)(exchangeDelta).scanLeft(firstOffer)(_ + _) :+ finalOffer
}

object Parameters {
  val default = Parameters()
}
