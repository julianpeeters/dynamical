package dynamical

import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}

class MonoSuite extends FunSuite:

    test("moore"):
      val m: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 4, 6)
      assertEquals(obtained, expected)

    test("mealy"):
      val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => if s then i + 1 else i, (s, i) => if i > 1 then true else s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[Int] = List(1, 2, 4)
      assertEquals(obtained, expected)

    test("wrap moore"):
      val l: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] =
        l.andThen(Wiring(i => j => j + j, (i1, i2) => i2)).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)

    test("wrapper"):
      val w: Wiring[Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = Wiring(i => j => j + j, (i1, i2) => i2)
      val l: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = l.andThen(w).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)