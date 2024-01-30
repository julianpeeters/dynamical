package dynamical

import cats.Id
import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial

class MonoSuite extends FunSuite:

    test("moore"):
      val m: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 4, 6)
      assertEquals(obtained, expected)

    test("mealy"):
      val m: Mealy[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Mealy(false, s => i => if s then i + 1 else i, (s, i) => if i > 1 then true else s)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[Int] = List(1, 2, 4)
      assertEquals(obtained, expected)

    test("wrap moore"):
      val l: Moore[Monomial.Store[Id[Boolean], _] ~> Monomial.Interface[Int, Id[Int], _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Monomial.Store[Id[Boolean], _] ~> Monomial.Interface[Int, Id[Int], _] ~> Monomial.Interface[Int, Int => Id[Int], _]] =
        l.andThen(Wiring(i => j => j + j, (i1, i2) => i2)).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)

    test("wrapper"):
      val w: Wiring[Monomial.Interface[Int, Id[Int], _] ~> Monomial.Interface[Int, Int => Id[Int], _]] = Wiring(i => j => j + j, (i1, i2) => i2)
      val l: Moore[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
      val m: Mealy[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int, _] ~> Monomial.Interface[Int, Int => Int, _]] = l.andThen(w).asMealy
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => m.run(s, i))._2   
      val expected: List[Int] = List(2, 4, 6) 
      assertEquals(obtained, expected)
  
    test("semi-choice"):
      val m: Moore[Monomial.Store[Option[Boolean], _] ~> Monomial.Interface[Int, Int => Int, _]] =
        Moore(Some(true), s => i => s.fold(i)(b => if b then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => !b))
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 20, 3)
      assertEquals(obtained, expected)

    test("wrapped semi-choice"):
      val w: Wiring[Monomial.Interface[Int, Id[Int], _] ~> Monomial.Interface[Int, Int => Id[Int], _]] = Wiring(i => j => j + j, (i1, i2) => i2)
      val n: Moore[Monomial.Store[Option[Boolean], _] ~> Monomial.Interface[Int, Id[Int], _]] =
        Moore(Some(true), s => s.fold(0)(b => if b then 1 else 0), (s, i) => if i > 1 then None else s.map(b => !b))
      val m = n.andThen(w)
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Int] = List(2, 4, 6)
      assertEquals(obtained, expected)  

    test("choice"):
      val m: Moore[Monomial.Store[Option[Boolean], _] ~> Monomial.Interface[Int, Int => Option[Int], _]] =
        Moore(Some(true), s => i => s.map(b => if b then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => !b))
      val obtained: List[Option[Int]] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Option[Int]] = List(Some(2), Some(20), None)
      assertEquals(obtained, expected)

    test("wrapped choice (prism composition)"):
      val w: Wiring[Monomial.Interface[Int, Int, _] ~> Monomial.Interface[Int, Int => Option[Int], _]] =
        Wiring(i => j => if j > 1 then Some(j + j) else Some(i), (i1, i2) => i2)
      val n: Moore[Monomial.Store[Option[Boolean], _] ~> Monomial.Interface[Int, Option[Int], _]] =
        Moore(Some(true), s => s.map(b => if b then 1 else 0), (s, i) => if i > 1 then None else s.map(b => !b))
      val m = n.andThen(w)
      val obtained: List[Option[Int]] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Option[Int]] = List(Some(1), Some(4), None)
      assertEquals(obtained, expected)

    test("semi-choice-strong"):
      val m: Moore[Monomial.Store[Option[(Int, Boolean)], _] ~> Monomial.Interface[Long, Long => Long, _]] =
        Moore(Some(0, true), s => i => s.fold(i)(b => if b._2 then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => b.copy(_2 = false)))
      val obtained: List[Long] = List(1L, 2L, 3L).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Long] = List(2L, 20L, 3L)
      assertEquals(obtained, expected)

    // test("semi-choice to strong wrapper"):
    //   // val w: Wiring[Monomial.Interface[Long, Option[(Int, Boolean)], _] ~> Monomial.Interface[Long, Long => Long, _]] = Wiring(i => j => j + j, (i1, i2) => i2)
    //   val w: Wiring[Monomial.Interface[Long, (Int, Boolean), _] ~> Monomial.Interface[Long, Long => Long, _]] = ???// Wiring(s => l => l, (o, l) => l)
    //   val n: Moore[Monomial.Store[Option[(Int, Boolean)], _] ~> Monomial.Interface[Long, Option[(Int, Boolean)], _]] =
    //     Moore(Some(1, true), s =>  s.map(b => if b._2 then b else (0, false)), (s, i) => if i > 1 then None else s.map(b => b.copy(_2 = false)))
    //   val m = n.andThen(w)
    //   val obtained: List[Long] = List(1L, 2L, 3L).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
    //   val expected: List[Long] = List(2L, 20L, 3L)
    //   assertEquals(obtained, expected)

    test("choice-strong"):
      val m: Moore[Monomial.Store[Option[(Int, Boolean)], _] ~> Monomial.Interface[Long, Long => Option[Long], _]] =
        Moore(Some((0, true)), s => i => s.map(b => if b._2 then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => b.copy(_2 = false)))
      val obtained: List[Option[Long]] = List(1L, 2L, 3L).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
      val expected: List[Option[Long]] = List(Some(2L), Some(20L), None)
      assertEquals(obtained, expected)



    // test("choice"):
    //   val m: Moore[Monomial.Store[Option[Boolean], _] ~> Monomial.Interface[Int, Int => Option[Int], _]] =
    //     Moore(Some(true), s => i => s.map(b => if b then i + i else 10*i), (s, i) => if i > 1 then None else s.map(b => !b))
    //   val obtained: List[Option[Int]] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
    //   val expected: List[Option[Int]] = List(Some(2), Some(20), None)
    //   assertEquals(obtained, expected)

    // test("strong"):
    //   val m: Moore[Monomial.Store[(Long, Boolean), _] ~> Monomial.Interface[Boolean, Boolean => Int, _]] = Moore((1L, false), s => i => if s._2 && i then 1 else 0, (s, i) => s.copy(_2 = i))
    //   val obtained: List[Int] = List(true, true, false).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
    //   val expected: List[Int] = List(0, 1, 0)
    //   assertEquals(obtained, expected)

    // test("strong wrapper"):
    //   val w: Wiring[Monomial.Interface[Boolean, Int, _] ~> Monomial.Interface[Boolean, Boolean => Int, _]] = Wiring(i => j => if j then i else 0, (i1, i2) => i2)
    //   val n: Moore[Monomial.Store[(Long, Boolean), _] ~> Monomial.Interface[Boolean, Int, _]] = Moore((1L, false), s => if s._2 then 1 else 0, (s, i) => s.copy(_2 = i))
    //   val m = n.andThen(w)
    //   val obtained: List[Int] = List(true, true, false).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
    //   val expected: List[Int] = List(0, 1, 0)
    //   assertEquals(obtained, expected)

    // test("choice"):
    //   val m: Moore[Monomial.Store[Option[Boolean], _] ~> Monomial.Interface[Int, Int => Option[Int], _]] = Moore(Some(true), s => i => s.map(b => if b then i + i else i), (s, i) => if i > 1 then None else s)
    //   val obtained: List[Option[Int]] = List(1, 2, 3).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
    //   val expected: List[Option[Int]] = List(Some(2), Some(4), None)
    //   assertEquals(obtained, expected)

    // test("choice wrapper"):
    //   val w: Wiring[Monomial.Interface[Int, Option[Int], _] ~> Monomial.Interface[Int, Int => Option[Int], _]] = Wiring(i => j => i.map(v => v + j), (i1, i2) => i2)
    //   val n: Moore[Monomial.Store[Option[Boolean], _] ~> Monomial.Interface[Int, Option[Int], _]] = Moore(Some(true), s => s.map(b => if b then 1 else 0), (s, i) => if i > 10 then None else s.map(b => !b))
    //   val m = n.andThen(w).asMealy
    //   val obtained: List[Option[Int]] = List(10, 20, 30).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
    //   val expected: List[Option[Int]] = List(Some(11), Some(20), None)
    //   assertEquals(obtained, expected)

    // test("choice strong"):
    //   val m: Moore[Monomial.Store[Option[(Int, Boolean)], _] ~> Monomial.Interface[Boolean, Boolean => Long, _]] = Moore(
    //     None,
    //     s => i => s match
    //       case None => 0L
    //       case Some(value) => if i then 1L else 0L,
    //     (s, i) => s match
    //       case None => Some((1, true))
    //       case Some(value) => if value._2 then Some(value.copy(_2 = false)) else 
    //   )
    //   val obtained: List[Long] = List(true, true, false).mapAccumulate(m.init)((s, i) => (m.update(s, i), m.readout(s)(i)))._2
    //   val expected: List[Long] = List(0L, 1L, 0L)
    //   assertEquals(obtained, expected)