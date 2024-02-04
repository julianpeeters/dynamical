package dynamical.fsm

import dynamical.fsm.methods.polymap.asMoore.asMoore
import dynamical.fsm.methods.types.{Init, Readout, Update}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Bi, Mono}
import polynomial.product.{◁, ⊗}

trait Moore[P[_]]:
  def init[Y]: Init[P, Y]
  def readout[Y]: Readout[P, Y]
  def update[Y]: Update[P, Y]

object Moore:

  export dynamical.fsm.methods.moore.andThen.*
  export dynamical.fsm.methods.moore.conversions.asMealy.*
  export dynamical.fsm.methods.moore.conversions.asPolyMap.*
  export dynamical.fsm.methods.moore.product.tensor.*
  export dynamical.fsm.methods.moore.swap.*

  def apply[S, A, B, Y](
    i: S,
    r: S => B,
    u: (S, A) => S
  ): Moore[Mono.Store[S, _] ~> Mono.Interface[A, B, _]] =
    PolyMap[Mono.Store[S, _], Mono.Interface[A, B, _], Y](r, u)
      .asMoore(i)

  @scala.annotation.targetName("applyComposite")
  def apply[S, A1, B1, A2, B2, Y](
    i: S,
    r: (S => B1, B1 => B2),
    u: (S, (A1, A2)) => S
  ): Moore[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, B2, _])] =
    PolyMap[Mono.Store[S, _], (Mono.Interface[A1, B1,_] ◁  Mono.Interface[A2, B2, _]), Y](r, u)
      .asMoore(i)
  
  def apply[S, A1, B1, A2, B2, Y](
    i: S,
    r1: S => B1,
    r2: S => B2,
    u1: (S, A1) => S,
    u2: (S, A2) => S
  ): Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _]] =
    PolyMap[Mono.Store[S, _],  Bi.Interface[A1, B1, A2, B2, _], Y]((r1, r2), (u1, u2))
      .asMoore(i)