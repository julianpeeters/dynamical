package dynamical

import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial
import polynomial.product.⊗

class TensorSuite extends FunSuite:

  test("moore tensor product"):
    val m1: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m2: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m3: Moore[(Monomial.Store[Boolean, _] ⊗ Monomial.Store[Boolean, _]) ~> (Monomial.Interface[Int, Int => Int, _] ⊗ Monomial.Interface[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(
      (s, a) => (m3.update(s, a), (m3.readout(s)._1(a._1), m3.readout(s)._2(a._2)))
    )._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("mealy tensor product"):
    val m1: Mealy[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m2: Mealy[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m3: Mealy[(Monomial.Store[Boolean, _]) ⊗ (Monomial.Store[Boolean, _]) ~> (Monomial.Interface[Int, Int => Int, _] ⊗ Monomial.Interface[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(m3.run)._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor product"):
    val w1: Wiring[Monomial.Interface[Int, Int, _] ~> Monomial.Interface[Int, Int => Int, _]] = Wiring(i => j => j + j, (i1, i2) => i2)
    val w2: Wiring[Monomial.Interface[Int, Int, _] ~> Monomial.Interface[Int, Int => Int, _]] = Wiring(i => j => j + j, (i1, i2) => i2)
    val w3: Wiring[(Monomial.Interface[Int, Int, _]) ⊗ (Monomial.Interface[Int, Int, _]) ~> (Monomial.Interface[Int, Int => Int, _] ⊗ Monomial.Interface[Int, Int => Int, _])] = w1 ⊗ w2
    val m1: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Monomial.Store[Boolean, _]) ⊗ (Monomial.Store[Boolean, _]) ~> (Monomial.Interface[Int, Int, _] ⊗ Monomial.Interface[Int, Int, _])] = (m1 ⊗ m2)
    val r: Mealy[
      (Monomial.Store[Boolean, _] ⊗ Monomial.Store[Boolean, _]) ~>
        ((Monomial.Interface[Int, Int, _] ⊗ Monomial.Interface[Int, Int, _])) ~>
          (Monomial.Interface[Int, Int => Int, _] ⊗ Monomial.Interface[Int, Int => Int, _])
    ] = m3.andThen(w3).asMealy
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(r.init)(r.run)._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor moore"):
    val m1: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Monomial.Store[Boolean, _] ⊗ Monomial.Store[Boolean, _]) ~> (Monomial.Interface[Int, Int, _] ⊗ Monomial.Interface[Int, Int, _])] = (m1 ⊗ m2)
    val n1: Wiring[(Monomial.Interface[Int, Int, _] ⊗ Monomial.Interface[Int, Int, _]) ~> (Monomial.Interface[Int, Int => Int, _])] = Wiring(b => a => a + a, (bb, a) => (bb._1 + a, bb._2 + a))
    val r: Mealy[(Monomial.Store[Boolean, _]) ⊗ (Monomial.Store[Boolean, _]) ~> (Monomial.Interface[Int, Int, _] ⊗ Monomial.Interface[Int, Int, _]) ~> Monomial.Interface[Int, Int => Int, _]] = m3.andThen(n1).asMealy
    val obtained: List[Int] = List(1, 2, 3).mapAccumulate(r.init)(r.run)._2
    val expected: List[Int] = List(2, 4, 6)
    assertEquals(obtained, expected)
    
  test("wiring"):
    type Plant[Y]      = Monomial.Interface[(Byte, Byte => Char), Char, Y]
    type Controller[Y] = Monomial.Interface[Char, Byte => Char, Y]
    type System[Y]     = Monomial.Interface[Byte, Byte => Char, Y]
    type ω[Y] = ((Plant ⊗ Controller) ~> System)[Y]
    val w: Wiring[ω] = Wiring(b => a => b._2(a), (b, a) => ((a, b._2), b._2(a)))
    val f1: Byte => Char = b => b.toChar
    val f2: Byte => Char = b => b.toChar.toUpper
    val m1: Moore[Monomial.Store[Char, _] ~> Plant] = Moore(" ".charAt(0), identity, (s, i) => i._2(i._1))
    val m2: Moore[Monomial.Store[Byte => Char, _] ~> Controller] = Moore(f1, identity, (f, i) => if i != ' ' then f else f2)
    val machine: Mealy[((Monomial.Store[Char, _] ⊗ Monomial.Store[Byte => Char, _]) ~> (Plant ⊗ Controller) ~> System)] = (m1 ⊗ m2).andThen(w).asMealy
    val obtained: String = "hello world".getBytes().toList.mapAccumulate(machine.init)(machine.run)._2.mkString
    val expected: String = "hello WORLD"
    assertEquals(obtained, expected)