package dynamical.fsm

import dynamical.fsm.Moore.asMoore
import dynamical.fsm.{Readout, Update}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Monomial, Store}

trait Wrapper[P[_]]:
  def `f₁`[Y]: Readout[P, Y]
  def `f#`[Y]: Update[P, Y]

object Wrapper:

  def apply[A, B, Y](
    r: B => A => B,
    u: (B, A) => A
  ): Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
    PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y](r, u).asWrapper

  extension [S, A, B, Y] (w: Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]])
    def asPolyMap: PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)
    def wrap(m: Moore[Store[S, _] ~> Monomial[A, B, _]]): Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)

  extension [S, A, B, Y] (p: PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y])
    def asWrapper: Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def `f₁`[Y]: Readout[Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.`φ#`
