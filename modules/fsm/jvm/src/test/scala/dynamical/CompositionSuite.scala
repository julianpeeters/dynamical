package dynamical

import cats.implicits.given
import dynamical.fsm.{Mealy, Moore}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.product.◁

class CompositionSuite extends FunSuite:

  test("mealy composition product"):
    type P[Y] = (Store[Boolean, _] ~> (Interface[Int, Int, _] ◁ Interface[String, String => String, _]))[Y]
    val n: Moore[P] =
      Moore(
        i = false,
        r = (
          (s: Boolean) => if s then 1 else 0,
          (i: Int) =>
            (a2: String) => if i == 1 then a2.toUpperCase else a2
        ),
        u = (s, a) =>
          if a._2 == "bar" then true else false // if we've seen a "bar", then the next should be capitialized
      )
    val m: Mealy[P] = n.asMealy
    val obtained: List[(Int, String)] = List((1, "foo"), (2, "bar"), (3, "baz")).mapAccumulate(m.init)(m.run)._2
    val expected: List[(Int, String)] = List((0, "foo"), (0, "bar"), (1, "BAZ"))
    assertEquals(obtained, expected)

