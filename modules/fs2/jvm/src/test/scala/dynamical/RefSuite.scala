package dynamical

import cats.effect.IO
import cats.effect.kernel.Ref
import dynamical.fsm.{Mealy, Moore, Wiring}
import dynamical.stream.transducer
import fs2.Stream
import munit.CatsEffectSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, StoreF}

class RefSuite extends CatsEffectSuite:

  test("ref"):
    val moore: IO[Moore[StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]]] =
      Ref.of[IO, Boolean](false).map(ref =>
        Moore(
          ref,
          s => s.get.map(b => if b then 1 else 0),
          (s, i) => s.update(b => b).as(s)
        )
      )
    val mealy: IO[Mealy[(StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]) ~> Interface[Int, Int => IO[Int], _]]] =
      moore.map(moore => moore.andThen(Wiring(i => j => i.as(j + j), (i1, i2) => i2)).asMealy)
    val obtained: IO[List[Int]] =
      for
        m <- mealy
        r <- Stream(1, 2, 3).covary[IO].through(m.transducer).compile.toList
      yield r
    val expected: List[Int] = List(2, 4, 6) 
    assertIO(obtained, expected)

  test("broadcast ref"):
    def moore(ref: Ref[IO, Boolean]): Moore[StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]] =
      Moore(
        ref,
        s => s.get.map(b => if b then 1 else 0),
        (s, i) => s.update(b => b).as(s)
      )
    def mealy(
      m: Moore[StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]]
    ): Mealy[(StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]) ~> Interface[Int, Int => IO[Int], _]] =
      m.andThen(Wiring(i => j => i.as(j + j), (i1, i2) => i2)).asMealy
    val obtained: IO[List[Int]] =
      for
        ref <- Ref.of[IO, Boolean](false)
        m1 = mealy(moore(ref))
        m2 = mealy(moore(ref))
        m3 = mealy(moore(ref))
        r <- Stream(1, 2, 3).covary[IO].broadcastThrough(m1.transducer, m2.transducer, m3.transducer).compile.toList
      yield r
    val expected: List[Int] = List(2, 2, 2, 4, 4, 4, 6, 6, 6) 
    assertIO(obtained, expected)

  test("merge and sort"):
    def moore(ref: Ref[IO, Boolean]): Moore[StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]] =
      Moore(
        ref,
        s => s.get.map(b => if b then 1 else 0),
        (s, i) => s.update(b => b).as(s)
      )
    def mealy(
      m: Moore[StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]]
    ): Mealy[(StoreF[IO, Ref[IO, Boolean], _] ~> Interface[Int, IO[Int], _]) ~> Interface[Int, Int => IO[Int], _]] =
      m.andThen(Wiring(i => j => i.as(j + j), (i1, i2) => i2)).asMealy
    val obtained: IO[List[Int]] =
      for
        ref <- Ref.of[IO, Boolean](false)
        m1 = mealy(moore(ref))
        m2 = mealy(moore(ref))
        r <- Stream(1, 2, 3).covary[IO].through(m1.transducer).merge(Stream(11, 22, 33).covary[IO].through(m2.transducer)).compile.toList
      yield r.sorted
    val expected: List[Int] = List(2, 4, 6, 22, 44, 66) 
    assertIO(obtained, expected)