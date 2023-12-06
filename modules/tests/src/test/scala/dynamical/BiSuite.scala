package dynamical

import cats.implicits.*
import dynamical.fsm.Moore
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.{Binomial, Store}

class BiSuite extends FunSuite:

    test("moore"):
      val m: Moore[
        Store[Boolean, _] ~>
          Binomial[
            Some[Int], Some[Int] => Some[Int],
            None.type, None.type => None.type,
            _
          ]
      ] =
        Moore(false, s => i => Some(i.value + i.value), s => i => i, (s, i) => s, (s, i) => s)
      val obtained: List[Option[Int]] = List(Some(1), None, Some(3)).mapAccumulate(m.init)((s, i) =>
        i match
          case v@Some(value) => (m.update._1(s, v), m.readout._1(s)(v))
          case None => (m.update._2(s, None), m.readout._2(s)(None))
        )._2
      val expected: List[Option[Int]] = List(Some(2), None, Some(6))
      assertEquals(obtained, expected)