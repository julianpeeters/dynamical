package dynamical

import cats.Id
import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.product.⊗

class TensorSuite extends FunSuite:

  test("moore tensor product"):
    val m1: Moore[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = Moore(false, s => i => i + i, (s, i) => s)
    val m3: Moore[(Store[Boolean, _] ⊗ Store[Boolean, _]) ~> (Interface[Int, Int => Int, _] ⊗ Interface[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(
      (s, a) => (m3.update(s, a), (m3.readout(s)._1(a._1), m3.readout(s)._2(a._2)))
    )._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("mealy tensor parallel"):
    val m1: Mealy[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m2: Mealy[Store[Boolean, _] ~> Interface[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val m3: Mealy[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Interface[Int, Int => Int, _] ⊗ Interface[Int, Int => Int, _])] = m1 ⊗ m2
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(m3.init)(m3.run)._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor product"):
    def w1[Y]: Wiring[Interface[Int, Int, _] ~> Interface[Int, Int => Int, _]] = Wiring[Id, Int, Int, Int, Int, Y](i => j => j + j, (i1, i2) => i2)
    def w2[Y]: Wiring[Interface[Int, Int, _] ~> Interface[Int, Int => Int, _]] = Wiring[Id, Int, Int, Int, Int, Y](i => j => j + j, (i1, i2) => i2)
    val w3: Wiring[(Interface[Int, Int, _]) ⊗ (Interface[Int, Int, _]) ~> (Interface[Int, Int => Int, _] ⊗ Interface[Int, Int => Int, _])] = w1 ⊗ w2
    val m1: Moore[Store[Boolean, _] ~> Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Interface[Int, Int, _] ⊗ Interface[Int, Int, _])] = (m1 ⊗ m2)
    val r: Mealy[
      (Store[Boolean, _] ⊗ Store[Boolean, _]) ~>
        ((Interface[Int, Int, _] ⊗ Interface[Int, Int, _])) ~>
          (Interface[Int, Int => Int, _] ⊗ Interface[Int, Int => Int, _])
    ] = m3.andThen(w3).asMealy
    val obtained: List[(Int, Int)] = List((1, 11), (2, 22), (3, 33)).mapAccumulate(r.init)(r.run)._2
    val expected: List[(Int, Int)] = List((2, 22), (4, 44), (6, 66))
    assertEquals(obtained, expected)

  test("wrapper tensor moore"):
    val m1: Moore[Store[Boolean, _] ~> Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m2: Moore[Store[Boolean, _] ~> Interface[Int, Int, _]] = Moore(false, s => if s then 1 else 0, (s, i) => s)
    val m3: Moore[(Store[Boolean, _] ⊗ Store[Boolean, _]) ~> (Interface[Int, Int, _] ⊗ Interface[Int, Int, _])] = (m1 ⊗ m2)
    val n1: Wiring[(Interface[Int, Int, _] ⊗ Interface[Int, Int, _]) ~> (Interface[Int, Int => Int, _])] = Wiring(b => a => a + a, (bb, a) => (bb._1 + a, bb._2 + a))
    val r: Mealy[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Interface[Int, Int, _] ⊗ Interface[Int, Int, _]) ~> Interface[Int, Int => Int, _]] = m3.andThen(n1).asMealy
    val obtained: List[Int] = List(1, 2, 3).mapAccumulate(r.init)(r.run)._2
    val expected: List[Int] = List(2, 4, 6)
    assertEquals(obtained, expected)
    
  test("wrapper tensor serial"):
    val m1: Moore[Store[Boolean, _] ~> Interface[Int, Long, _]] = Moore(false, s => if s then 1L else 0L, (s, i) => if i > 1 then true else false)
    val m2: Moore[Store[Boolean, _] ~> Interface[Long, String, _]] = Moore(false, s => if s then "true" else "false", (s, i) => if i > 0L then true else false)
    val m3: Moore[(Store[Boolean, _] ⊗ Store[Boolean, _]) ~> (Interface[Int, Long, _] ⊗ Interface[Long, String, _])] = (m1 ⊗ m2)
    val n1: Wiring[(Interface[Int, Long, _] ⊗ Interface[Long, String, _]) ~> (Interface[Int, Int => String, _])] = Wiring(ba => a => ba._2, (ba, a) => (a, ba._1))
    val r: Mealy[(Store[Boolean, _]) ⊗ (Store[Boolean, _]) ~> (Interface[Int, Long, _] ⊗ Interface[Long, String, _]) ~> Interface[Int, Int => String, _]] = m3.andThen(n1).asMealy
    val obtained: List[String] = List(1, 2, 3, 4).mapAccumulate(r.init)(r.run)._2
    val expected: List[String] = List("false", "false", "false", "true")
    assertEquals(obtained, expected)
    
  test("wiring"):
    type Plant[Y]      = Interface[(Byte, Byte => Char), Char, Y]
    type Controller[Y] = Interface[Char, Byte => Char, Y]
    type System[Y]     = Interface[Byte, Byte => Char, Y]
    type ω[Y] = ((Plant ⊗ Controller) ~> System)[Y]
    val w: Wiring[ω] = Wiring(b => a => b._2(a), (b, a) => ((a, b._2), b._2(a)))
    val f1: Byte => Char = b => b.toChar
    val f2: Byte => Char = b => b.toChar.toUpper
    val m1: Moore[Store[Char, _] ~> Plant] = Moore(" ".charAt(0), c => c, (s, i) => i._2(i._1))
    val m2: Moore[Store[Byte => Char, _] ~> Controller] = Moore(f1, c => c, (f, i) => if i != ' ' then f else f2)
    val machine: Mealy[((Store[Char, _] ⊗ Store[Byte => Char, _]) ~> (Plant ⊗ Controller) ~> System)] = (m1 ⊗ m2).andThen(w).asMealy
    val obtained: String = "hello world".getBytes().toList.mapAccumulate(machine.init)(machine.run)._2.mkString
    val expected: String = "hello WORLD"
    assertEquals(obtained, expected)

  test("discard based on output"):
    type Supplier1[Y] = Interface[Unit, Int, Y]
    type Supplier2[Y] = Interface[Unit, Int, Y]
    type Company[Y]   = Interface[Int, Boolean, Y]
    type System[Y]    = Interface[Unit, Unit => Boolean, Y]
    val w: Wiring[((Company ⊗ Supplier1 ⊗ Supplier2) ~> System)] = Wiring(b => a => b._1._1, (b, a) => ((if b._1._1 then b._2 else b._1._2, a), a))
    val m0: Moore[Store[Int, _] ~> Company] = Moore(0, s => if s > 2 then true else false, (s, i) =>  s + i) // always accumulate state
    val m1: Moore[Store[Unit, _] ~> Supplier1] = Moore((), _ => 1, (s, _) => s)                              // emit 1s
    val m2: Moore[Store[Unit, _] ~> Supplier2] = Moore((), _ => 0, (s, _) => s)                              // emit 0s
    val mealy: Mealy[(Store[Int, _] ⊗ Store[Unit, _] ⊗ Store[Unit, _]) ~> (Company ⊗ Supplier1 ⊗ Supplier2) ~> System] = (m0 ⊗ m1 ⊗ m2).andThen(w).asMealy
    val obtained: List[Boolean] = List((), (), () ,()).mapAccumulate(mealy.init)(mealy.run)._2 // switch suppliers when stock exceeds 2
    val expected: List[Boolean] = List(false, false, false, true)
    assertEquals(obtained, expected)