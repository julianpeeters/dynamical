package dynamical

import cats.implicits.given
import dynamical.fsm.{Mealy, Moore}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial
import polynomial.product.◁

class CompositionSuite extends FunSuite:

  test("mealy composition product"):
    type P[Y] = (Monomial.Store[Boolean, _] ~> (Monomial.Interface[Int, Int => Int, _] ◁ Monomial.Interface[String, String => String, _]))[Y]
    val n: Moore[P] =
      Moore(
        i = false,
        r = (
          (s: Boolean) =>
            // if the previous pair contained a "foo", then the current pair's int will be multiplied by 10
            if s then (i: Int) => i * 10 else (i: Int) => i,
          (f: Int => Int) =>
            (a2: String) => a2
        ),
        u = (s, a) =>
          if a._2 == "foo" then true else false
      )
    val m: Mealy[P] = n.asMealy
    val obtained: List[(Int, String)] = List((1, "foo"), (2, "bar"), (3, "foo"), (4, " baz")).mapAccumulate(m.init)(m.run)._2
    val expected: List[(Int, String)] = List((1, "foo"), (20, "bar"), (3, "foo"), (40, " baz"))
    assertEquals(obtained, expected)

