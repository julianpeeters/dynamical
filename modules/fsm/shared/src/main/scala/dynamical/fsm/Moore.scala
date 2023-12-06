package dynamical.fsm

import dynamical.fsm.{Init, Readout, Update}
import polynomial.morphism.PolyMap
import polynomial.`object`.{Binomial, Monomial, Store}
import polynomial.morphism.~>

trait Moore[P[_]]:
  def init[Y]: Init[P, Y]
  def readout[Y]: Readout[P, Y]
  def update[Y]: Update[P, Y]

object Moore:

  def apply[S, A, B, Y](
    i: S,
    r: S => B,
    u: (S, A) => S
  ): Moore[Store[S, _] ~> Monomial[A, B, _]] =
    PolyMap[Store[S, _], Monomial[A, B, _], Y](r, u)
      .asMoore(i)
  
  def apply[S, A1, B1, A2, B2, Y](
    i: S,
    r1: S => B1,
    r2: S => B2,
    u1: (S, A1) => S,
    u2: (S, A2) => S
  ): Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]] =
    PolyMap[Store[S, _],  Binomial[A1, B1, A2, B2, _], Y]((r1, r2), (u1, u2))
      .asMoore(i)

  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Monomial[A, B, _], Y])
    @scala.annotation.targetName("asMooreMono")
    def asMoore(i: S): Moore[Store[S, _] ~> Monomial[A, B, _]] =
      new Moore[Store[S, _] ~> Monomial[A, B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> Monomial[A, B, _], Y] =
          p.`φ#`

  extension [S, A, B, Y] (p: PolyMap[PolyMap[Store[S, _], Monomial[A, B, _], _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asMooreMonoMono")
    def asMoore(i: S): Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Store[S, _], Binomial[A1, B1, A2, B2, _], Y])
    @scala.annotation.targetName("asMooreBi")
    def asMoore(i: S): Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]] =
      new Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]]:
        def init[Y]: Init[Store[S, _] ~> Binomial[A1, B1, A2, B2, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Binomial[A1, B1, A2, B2, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> Binomial[A1, B1, A2, B2, _], Y] =
          p.`φ#`

  extension [S, A, B, Y] (m: Moore[Store[S, _] ~> Monomial[A, B, _]])
    def asPolyMap: PolyMap[Store[S, _], Monomial[A, B, _], Y] =
      PolyMap(
        m.readout, 
        m.update
      )      

  extension [S, A, B, Y] (m: Moore[Store[S, _] ~> Monomial[A, B, _]])
    def wrapped(
      r: B => A => B,
      u: (B, A) => A
    ): Moore[(Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _])] =
      PolyMap[PolyMap[Store[S, _], Monomial[A, B, _], _], Monomial[A, A => B, _], Y](
        m.readout.andThen(r),
        (s, a) => m.update(s, u(m.readout(s), a))
      ).asMoore(m.init)