package dynamical

import cats.implicits.*
import munit.FunSuite
import dynamical.fsm.{Mealy, Moore, Wrapper}
import dynamical.fsm.Mealy.asMealy
import polynomial.morphism.~>
import polynomial.`object`.{Monomial, Store}

class DynamicalSuite extends FunSuite:

    test("moore"):
      val m: Moore[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 4, 6)
      assertEquals(obtained, expected)

    test("mealy"):
      val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[Int] = List(2, 4, 6)
      assertEquals(obtained, expected)

    test("wrap mono"):
      val l: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] =
        l.wrapped(i => j => j + j, (i1, i2) => i2).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)

    test("wrap poly"):
      val w: Wrapper[Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = Wrapper(i => j => j + j, (i1, i2) => i2)
      val l: Moore[Store[Boolean, _] ~> Monomial[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Store[Boolean, _] ~> Monomial[Int, Int, _] ~> Monomial[Int, Int => Int, _]] = w.wrap(l).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)