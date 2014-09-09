package com.coinffeine.lineage

import org.scalatest.{FlatSpec, ShouldMatchers}

class LineagedTest extends FlatSpec with ShouldMatchers {
  val num = implicitly[Fractional[Lineaged[Double]]]
  import num._

  "Lineage expressions" should "be formed from a variable" in {
    val exp = Lineaged.variable('x, 10d)
    exp.value shouldBe 10d
    exp.expression.toString should be ("x")
  }

  it should "be formed from a constant value" in {
    val exp = Lineaged.value(10d)
    exp.value shouldBe 10d
    exp.expression.toString should be ("10.0")
  }

  it should "be formed adding expressions" in {
    val exp = Lineaged.variable('x, 5d) + Lineaged.value(10d)
    exp.value shouldBe 15d
    exp.expression.toString should be ("(x + 10.0)")
  }

  it should "reject to be formed from expressions with conflicting bindings" in {
    an [IllegalArgumentException] shouldBe thrownBy {
      Lineaged.variable('x, 5d) + Lineaged.variable('x, 10d)
    }
  }

  it should "be formed subtracting expressions" in {
    val exp = Lineaged.value(5d) - Lineaged.value(10d)
    exp.value shouldBe -5d
    exp.expression.toString should be ("(5.0 - 10.0)")
  }

  it should "be formed multiplying expressions" in {
    val exp = Lineaged.variable('x, 5d) * Lineaged.variable('y, 10)
    exp.value shouldBe 50d
    exp.expression.toString should be ("(x * y)")
  }

  it should "be formed dividing expressions" in {
    val exp = Lineaged.variable('x, 10d) / Lineaged.value(2)
    exp.value shouldBe 5d
    exp.expression.toString should be ("(x / 2.0)")
  }

  it should "be formed by negating expressions" in {
    val exp = num.negate(Lineaged.variable('x, 3))
    exp.value shouldBe -3d
    exp.expression.toString should be ("-x")
  }

  it should "compare expressions" in {
    val e1 = Lineaged.value(3.3d)
    val e2 = Lineaged.variable('x, 1d) * Lineaged.value(3d)
    e1 should be > e2
  }

  it should "be simplified when expressions are grounded" in {
    val ex = num.negate(Lineaged.value(1d) + Lineaged.value(2d)) * Lineaged.value(3d) / Lineaged.value(4d)
    ex.simplified shouldBe Lineaged.value(-2.25d)
  }
}
