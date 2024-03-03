package dynamical.fsm

import dynamical.fsm.methods.types.Run
import dynamical.fsm.methods.mealy.run.Runner2
import dynamical.fsm.methods.polymap.asMealy.asMealy
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store}

trait Mealy[P[_]] extends Moore[P]:
  def run[Y]: Run[P, Y]

object Mealy:

  export dynamical.fsm.methods.mealy.product.tensor.*

  def apply[S, A, B, Y](
    i: S,
    r: S => A => B,
    u: (S, A) => S
  ): Mealy[Store[S, _] ~> Interface[A, A => B, _]] =
    PolyMap[Store[S, _], Interface[A, A => B, _], Y](r, u).asMealy(i)

  def apply[S, A1, B1, A2, B2, Y](
    i: S,
    r1: S => A1 => B1,
    r2: S => A2 => B2,
    u1: (S, A1) => S,
    u2: (S, A2) => S
  )(
    using Runner2[Store[S, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _], S, A1, B1, A2, B2]
  ): Mealy[Store[S, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]] =
    PolyMap[Store[S, _], BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y]((r1, r2), (u1, u2)).asMealy(i)
