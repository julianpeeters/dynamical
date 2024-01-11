package dynamical

import dynamical.fsm.Mealy
import dynamical.stream.transducer
import fs2.Stream
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial

class fs2Suite extends FunSuite:

  test("fs2"):
    val m: Mealy[Monomial.Store[Boolean, _] ~> Monomial.Interface[Int, Int => Int, _]] = Mealy(false, s => i => i + i, (s, i) => s)
    val obtained: List[Int] = Stream(1, 2, 3).through(m.transducer).compile.toList
    val expected: List[Int] = Stream(2, 4, 6).compile.toList
    assertEquals(obtained, expected)