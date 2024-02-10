package dynamical

import cats.Id
import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}

class MonoSuite extends FunSuite:

    test("moore"):
      val m: Moore[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 4, 6)
      assertEquals(obtained, expected)

    test("mealy"):
      val m: Mealy[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = Mealy(false, s => i => if s then i + 1 else i, (s, i) => if i > 1 then true else s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[Int] = List(1, 2, 4)
      assertEquals(obtained, expected)

    test("wrap moore"):
      val l: Moore[Store[Boolean, _] ~> Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Store[Boolean, _] ~> Interface[Int, Int, _] ~> Interface[Int, Int => Int, _]] =
        l.andThen(Wiring(i => j => j + j, (i1, i2) => i2)).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)

    test("wrapper"):
      def w[Y]: Wiring[Interface[Int, Int, _] ~> Interface[Int, Int => Int, _]] = Wiring[Id, Int, Int, Y](i => j => j + j, (i1, i2) => i2)
      val l: Moore[Store[Boolean, _] ~> Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Store[Boolean, _] ~> Interface[Int, Int, _] ~> Interface[Int, Int => Int, _]] = l.andThen(w).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)
  
    test("semi-choice"):
      val m: Moore[Store[Option[Boolean], _] ~> Interface[Int, Int => Int, _]] =
        Moore(Some(true), s => i => s.fold(i)(b => if b then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => !b))
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 20, 3)
      assertEquals(obtained, expected)

    test("wrapped semi-choice"):
      def w[Y]: Wiring[Interface[Int, Int, _] ~> Interface[Int, Int => Int, _]] = Wiring[Id, Int, Int, Y](i => j => j + j, (i1, i2) => i2)
      val n: Moore[Store[Option[Boolean], _] ~> Interface[Int, Int, _]] =
        Moore(Some(true), s => s.fold(0)(b => if b then 1 else 0), (s, i) => if i > 1 then None else s.map(b => !b))
      val m = n.andThen(w)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 4, 6)
      assertEquals(obtained, expected)  

    test("choice"):
      val m: Moore[Store[Option[Boolean], _] ~> Interface[Int, Int => Option[Int], _]] =
        Moore(Some(true), s => i => s.map(b => if b then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => !b))
      val obtained: List[Option[Int]] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Option[Int]] = List(Some(2), Some(20), None)
      assertEquals(obtained, expected)

    test("wrapped choice (prism composition)"):
      val w: Wiring[Interface[Int, Int, _] ~> Interface[Int, Int => Option[Int], _]] =
        Wiring(i => j => if j > 1 then Some(j + j) else Some(i), (i1, i2) => i2)
      val n: Moore[Store[Option[Boolean], _] ~> Interface[Int, Option[Int], _]] =
        Moore(Some(true), s => s.map(b => if b then 1 else 0), (s, i) => if i > 1 then None else s.map(b => !b))
      val m = n.andThen(w)
      val obtained: List[Option[Int]] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Option[Int]] = List(Some(1), Some(4), None)
      assertEquals(obtained, expected)

    test("semi-choice-strong"):
      val m: Moore[Store[Option[(Int, Boolean)], _] ~> Interface[Long, Long => Long, _]] =
        Moore(Some(0, true), s => i => s.fold(i)(b => if b._2 then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => b.copy(_2 = false)))
      val obtained: List[Long] = List(1L, 2L, 3L).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Long] = List(2L, 20L, 3L)
      assertEquals(obtained, expected)

    test("choice-strong"):
      val m: Moore[Store[Option[(Int, Boolean)], _] ~> Interface[Long, Long => Option[Long], _]] =
        Moore(Some((0, true)), s => i => s.map(b => if b._2 then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => b.copy(_2 = false)))
      val obtained: List[Option[Long]] = List(1L, 2L, 3L).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Option[Long]] = List(Some(2L), Some(20L), None)
      assertEquals(obtained, expected)

