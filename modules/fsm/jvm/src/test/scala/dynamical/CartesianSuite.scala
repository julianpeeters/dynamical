package dynamical

import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.product.×

class CartesianSuite extends FunSuite:

  test("mealy cartesian product"):
    type P[Y] = (Store[Boolean, _] ~> (Interface[Int, Int, _] × Interface[String, String, _]))[Y]
    val n: Moore[P] =
      Moore(
        i = false,
        r = (s: Boolean) => (if s then 1 else 0, "hello"),
        u = (s: Boolean, a: Either[Int, String]) => a match
          case Left(value) => !s
          case Right(value) => s
        ,
      )
    val w: Wiring[(Interface[Int, Int, _] × Interface[String, String, _]) ~> Interface[Either[Int, String], Either[Int, String] => (Int, String), _]] =
      Wiring(
        r = b => a => b,
        u = (b, a) => a
      )
    val m: Mealy[Store[Boolean, _] ~> (Interface[Int, Int, _] × Interface[String, String, _]) ~> Interface[Either[Int, String], Either[Int, String] => (Int, String), _]] =
      n.andThen(w).asMealy
    val obtained: List[(Int, String)] = List(Right("foo"), Left(2), Right("bar")).mapAccumulate(m.init)(m.run)._2
    val expected: List[(Int, String)] = List((0, "hello"), (0, "hello"), (1, "hello"))
    assertEquals(obtained, expected)

