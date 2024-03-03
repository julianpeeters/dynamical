package dynamical.fsm

import cats.Id
import dynamical.fsm.methods.polymap.asWiring.asWiring
import dynamical.fsm.methods.types.{Readout, Update}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.Interface
import polynomial.product.{×, ⊗}

trait Wiring[P[_]]:
  def `f₁`[Y]: Readout[P, Y]
  def `f#`[Y]: Update[P, Y]

object Wiring:

  export dynamical.fsm.methods.wiring.product.tensor.*
  export dynamical.fsm.methods.wiring.asPolyMap.*

  @scala.annotation.targetName("appMono")
  def apply[A, B, C, D, Y](
    r: B => C => D,
    u: (B, C) => A
  ): Wiring[Interface[A, B, _] ~> Interface[C, C => Id[D], _]] =
    PolyMap[Interface[A, B, _], Interface[C, C => Id[D], _], Y](r, u).asWiring

  @scala.annotation.targetName("appMonoF")
  def apply[F[_], A, B, C, D, Y](
    r: B => C => F[D],
    u: (B, C) => A
  ): Wiring[Interface[A, B, _] ~> Interface[C, C => F[D], _]] =
    PolyMap[Interface[A, B, _], Interface[C, C => F[D], _], Y](r, u).asWiring

  @scala.annotation.targetName("appBi")
  def apply[A1, B1, A2, B2, Y](
    r1: B1 => A1 => B1,
    r2: B2 => A2 => B2,
    u1: (B1, A1) => A1,
    u2: (B2, A2) => A2
  ): Wiring[BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]] =
    PolyMap[BiInterface[A1, B1, A2, B2, _], BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y]((r1, r2), (u1, u2)).asWiring

  @scala.annotation.targetName("appCartesian1")
  def apply[A1, B1, A2, B2, Y](
    r: ((B1, B2)) => Either[A1, A2] => (B1, B2),
    u: ((B1, B2), Either[A1, A2]) => Either[A1, A2]
  ): Wiring[(Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]] =
    PolyMap[(Interface[A1, B1, _] × Interface[A2, B2, _]), Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y](r, u).asWiring

  @scala.annotation.targetName("appTensored1")
  def apply[A1, B1, A2, B2, Y](
    r: ((B1, B2)) => A1 => B2,
    u: ((B1, B2), A1) => (A1, A2)
  ): Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]] =
    PolyMap[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), Interface[A1, A1 => B2, _], Y](r, u).asWiring

  @scala.annotation.targetName("appTensored2")
  def apply[A1, B1, A2, B2, A3, B3, I, O, Y](
    r: (((B1, B2), B3)) => I => O,
    u: (((B1, B2), B3), I) => ((A1, A2), A3)
  ): Wiring[
    (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~>
      Interface[I, I => O, _]
    ] =
      PolyMap[
        (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]),
        Interface[I, I => O, _],
        Y
      ](r, u).asWiring

  @scala.annotation.targetName("appTensored3")
  def apply[A, B, C, Y](
    r: ((C, B)) => A => C,
    u: ((C, B), A) => ((A, B), C)
  ): Wiring[
    (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~>
      Interface[A, A => C, _]
    ] =
      PolyMap[
        (Interface[(A, B), C, _] ⊗ Interface[C, B, _]),
        Interface[A, A => C, _],
        Y
      ](r, u).asWiring  

  @scala.annotation.targetName("appBiTensored1")
  def apply[A1, B1, A2, B2, A3, B3, A4, B4, Y](
    r1: ((B1, B3)) => A1 => B3,
    r2: ((B2, B4)) => A2 => B4,
    u1: ((B1, B3), A1) => (A1, A3),
    u2: ((B2, B4), A2) => (A2, A4)
  ): Wiring[
    (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>
      BiInterface[A1, A1 => B3, A2, A2 => B4, _]
    ] =
      PolyMap[
        (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]),
        BiInterface[A1, A1 => B3, A2, A2 => B4, _],
        Y
      ]((r1, r2), (u1, u2)).asWiring

  @scala.annotation.targetName("appTensored4")
  def serially[A1, B1, A2, B2, B3, Y]: Wiring[
    (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[B1, B1, B2, B3, _]) ~>
      BiInterface[A1, A1 => B1, A2, A2 => B3, _]
    ] =
      PolyMap[
        (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[B1, B1, B2, B3, _]),
        BiInterface[A1, A1 => B1, A2, A2 => B3, _],
        Y
      ](
        (
          (_, b1) => a1 => b1,
          (_, b3) => a2 => b3
        ),(
          (b, a1) => (a1, b._2), 
          (b, a2) => (a2, b._1)
        )
      ).asWiring