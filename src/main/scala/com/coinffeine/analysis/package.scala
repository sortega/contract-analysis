package com.coinffeine

import com.coinffeine.lineage.Lineaged

package object analysis {
  //type Payoff = BigDecimal

  type Payoff = Lineaged[BigDecimal]
}
