package dynamical

import cats.implicits.*
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}
import polynomial.product.⊗

class TensorSuite extends FunSuite:

  test("moore tensor product"):
    val m1: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m3: Moore[(Store[Boolean, _] ⊗ Store[Boolean, _]) ~> (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(
      (s, a) => (m3.update(s, a), (m3.readout(s)._1(a._1), m3.readout(s)._2(a._2)))
    )._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("mealy tensor product"):
    val m1: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m2: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m3: Mealy[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(m3.run)._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor product"):
    val w1: Wiring[Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = Wiring(i => j => j + j, (i1, i2) => i2)
    val w2: Wiring[Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = Wiring(i => j => j + j, (i1, i2) => i2)
    val w3: Wiring[(Monomial[Int, Int, _]) ⊗ (Monomial[Int, Int, _]) ~> (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])] = w1 ⊗ w2
    val m1: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _])] = (m1 ⊗ m2)
    val r: Mealy[
      (Store[Boolean, _] ⊗ Store[Boolean, _]) ~>
        ((Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _])) ~>
          (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])
    ] = m3.andThen(w3).asMealy
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(r.init)(r.run)._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor moore"):
    val m1: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Store[Boolean, _] ⊗ Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _])] = (m1 ⊗ m2)
    val n1: Wiring[(Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _]) ~> (Monomial[Int, Int => Int, _])] = Wiring(b => a => a + a, (bb, a) => (bb._1 + a, bb._2 + a))
    val r: Mealy[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _]) ~> Monomial[Int, Int => Int, _]] = m3.andThen(n1).asMealy
    val obtained: List[Int] = List(1, 2, 3).mapAccumulate(r.init)(r.run)._2
    val expected: List[Int] = List(2, 4, 6)
    assertEquals(obtained, expected)

  // test("wiring"):
  //   type Plant[T, U, V, Y]   = Monomial[(T, U), V, Y]
  //   type Controller[T, U, Y] = Monomial[U, T, Y]
  //   type System[T, U, Y]     = Monomial[T, U, Y]

  //   def w1[A, B, C, Y]: Wiring[(Plant[A, B, C, _] ⊗ Controller[B, C, _]) ~> System[A, C, _]] =
  //     ???
  //   val _ = w1

  //   def machine[S1, S2, A, B, C]: Mealy[((Store[S1, _] ⊗ Store[S2, _]) ~> (Plant[A, B, C, _] ⊗ Controller[B, C, _]) ~> System[A, C, _])] =
  //     ???

  //   val _ = machine
  //   val m1: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
  //   val m2: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
  //   val m3: Moore[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _])] = (m1 ⊗ m2)
  //   val n1: Wiring[(Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _]) ~> (Monomial[Int, Int => Int, _])] = Wiring(b => a => a + a, (bb, a) => (bb._1 + a, bb._2 + a))
  //   val r: Mealy[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _]) ~> Monomial[Int, Int => Int, _]] = m3.andThen(n1).asMealy
  //   val obtained: List[Int] = List(1, 2, 3).mapAccumulate(r.init)(r.run)._2
  //   val expected: List[Int] = List(2, 4, 6)
  //   assertEquals(obtained, expected)