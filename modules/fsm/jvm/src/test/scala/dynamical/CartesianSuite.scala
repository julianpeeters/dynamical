package dynamical

import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.product.{×, ⊗}

class CartesianSuite extends FunSuite:

  test("mealy cartesian product"):

    val n: Moore[(Store[Boolean, _] ~> (Interface[Int, Int, _] × Interface[String, String, _]))] =
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

  test("mealy cartesian tensored"):
    val n: Moore[Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])] =
      Moore(
        i = "",
        r = s => ((s.length, s), s.length >= 2),
        u = (s, a) => if a._2 then s else a._1 match
          case Left(value) => value.toString
          case Right(value) => s + value,
      )
    val w: Wiring[
      ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _]) ~>
        Interface[((Either[Int, String], Boolean)), ((Either[Int, String], Boolean)) => ((Int, String), Boolean), _]
      ] =
      Wiring(
        r = b => a => b,
        (b, a) => a
      )
    val m: Mealy[Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _]) ~> Interface[((Either[Int, String], Boolean)), ((Either[Int, String], Boolean)) => ((Int, String), Boolean), _]] =
      n.andThen(w).asMealy
    val obtained: List[((Int, String), Boolean)] = List((Right("foo"), true), (Left(2), false), (Right("bar"), true)).mapAccumulate(m.init)(m.run)._2
    val expected: List[((Int, String), Boolean)] = List(((0, ""), false), ((0, ""), false), ((1, "2"), false))
    assertEquals(obtained, expected)

  test("mealy tensored cartesian"):
    val n: Moore[Store[String, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))] =
      Moore(
        i = "",
        r = s => (s.length, (s, s.length > 3)),
        u = (s, a) => a match
          case Left(value) => s + "!"
          case Right(value) => s + value._1
      )
    val w: Wiring[
      (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _])) ~>
        Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _]
      ] =
      Wiring(
        r = b => a => b,
        (b, a) => a
      )
    val m: Mealy[
      Store[String, _] ~>
        (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _])) ~>
          Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _]
      ] =
      n.andThen(w).asMealy
    val obtained: List[(Int, (String, Boolean))] = List(Right(("foo", true)), Left(2), Right(("bar", false))).mapAccumulate(m.init)(m.run)._2
    val expected: List[(Int, (String, Boolean))] = List((0, ("", false)), (3, ("foo", false)), (4, ("foo!", true)))
    assertEquals(obtained, expected)

  test("mealy linearly distributive"):
    type P[Y] = Interface[Int, Double, Y]
    type Q[Y] = Interface[String, Char, Y]
    type R[Y] = Interface[Boolean, Unit, Y]
    type S[Y] = Store[String, Y]
    type T[Y] = Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Double, (Char, Unit)), Y]
    val n: Moore[S ~> ((P × Q) ⊗ R)] =
      Moore(
        i = "",
        r = s => ((s.length, s.headOption.getOrElse('!')), ()),
        u = (s, a) => if a._2 then s else a._1 match
          case Left(value) => value.toString
          case Right(value) => s + value,
      )
    val w: Wiring[((P × Q) ⊗ R) ~> (P × (Q ⊗ R))] =
      Wiring(
        b => (b._1._1, (b._1._2, b._2)),                  // re-tuple,
        (b, a) => a match                                 // exhibit strength:
          case Left(value) => (Left(value), value < 1)
          case Right(value) => (Right(b._1._2+:value._1), value._2)
      )
    val z: Wiring[(P × (Q ⊗ R)) ~> T] = Wiring(b => a => b, (b, a) => a)
    val m: Mealy[((S ~> ((P × Q) ⊗ R)) ~> (P × (Q ⊗ R))) ~> T] = n.andThen(w).andThen(z).asMealy
    val obtained: List[(Double, (Char, Unit))] = List(Right(("foo", true)), Left(2), Right(("bar", false))).mapAccumulate(m.init)(m.run)._2
    val expected: List[(Double, (Char, Unit))] = List((0, ('!', ())), (0, ('!', ())), (1, ('2', ())))
    assertEquals(obtained, expected)