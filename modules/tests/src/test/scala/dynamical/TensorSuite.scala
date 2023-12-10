package dynamical

import cats.implicits.*
import munit.FunSuite
import dynamical.fsm.{Mealy, Moore, Wrapper}
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}
import polynomial.⊗

class TensorSuite extends FunSuite:

  test("moore tensor product"):
    val m1: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m3: Moore[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(
      (s, a) => (m3.update(s, a), (m3.readout(s)._1(a._1), m3.readout(s)._2(a._2)))
    )._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("mealy tensor product"):
    val m1: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m2: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m3: Mealy[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(
      (s, a) => (m3.update(s, a), (m3.readout(s)._1(a._1), m3.readout(s)._2(a._2)))
    )._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor product"):
    val w1: Wrapper[Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = Wrapper(i => j => j + j, (i1, i2) => i2)
    val w2: Wrapper[Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = Wrapper(i => j => j + j, (i1, i2) => i2)
    val w3: Wrapper[(Monomial[Int, Int, _]) ⊗ (Monomial[Int, Int, _]) ~> (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])] = w1 ⊗ w2
    val m1: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _])] = (m1 ⊗ m2)
    val r: Moore[
      (Store[Boolean, _] ⊗ Store[Boolean, _]) ~>
        ((Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _])) ~>
          (Monomial[Int, Int => Int, _] ⊗ Monomial[Int, Int => Int, _])
    ] = w3.wrap(m3)
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(r.init)(
      (s, a) => (r.update(s, a), (r.readout(s)._1(a._1), r.readout(s)._2(a._2)))
    )._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor moore"):
    val m1: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _])] = (m1 ⊗ m2)
    val n1: Wrapper[(Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _]) ~> (Monomial[Int, Int => Int, _])] = Wrapper(b => a => a + a, (bb, a) => (bb._1 + a, bb._2 + a))
    val r: Moore[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Monomial[Int, Int, _] ⊗ Monomial[Int, Int, _]) ~> Monomial[Int, Int => Int, _]] = n1.wrap(m3)
    val obtained: List[Int] = List(1, 2, 3).mapAccumulate(r.init)((s, a) => (r.update(s, a), r.readout(s)(a)))._2
    val expected: List[Int] = List(2, 4, 6)
    assertEquals(obtained, expected)
