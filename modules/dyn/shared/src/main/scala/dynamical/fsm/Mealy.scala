package dynamical.fsm

import polynomial.functor.{Dir, Pos}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Monomial, Store}
import dynamical.fsm.{Init, Readout, Update}

trait Mealy[P[_]] extends Moore[P]:
  def run[Y]: Mealy.Run[P, Y]

object Mealy:

  def apply[S, A, B, Y](
    i: S,
    r: S => A => B,
    u: (S, A) => S
  ): Mealy[Store[S, _] ~> Monomial[A, A => B, _]] =
    PolyMap[Store[S, _], Monomial[A, A => B, _], Y](r, u).asMealy(i)

  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asMealy1")
    def asMealy(i: S): Mealy[Store[S, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[(Store[S, _]) ~> Monomial[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          p.φ
        def update[Y]: Update[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          (s, a) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, B, Y] (p: (Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _])[Y])
    @scala.annotation.targetName("asMealy2")
    def asMealy(i: S): Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[(Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          (s, a) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, B, Y] (p: Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]])
    @scala.annotation.targetName("asMealy3")
    def asMealy: Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[(Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          p.update
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))


  type Run[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => (p[Y], q[Y]) match
      case (Store[s, Y], Monomial[a, ? => b, Y]) => (s, a) => (s, b)
      case (Store[s, Y], PolyMap[p, Monomial[a, ? => b, _], Y]) => (s, a) => (s, b)
      case (PolyMap[Store[s, _], ?, Y], Monomial[a, ? => b, Y]) => (s, a) => (s, b)
      case (PolyMap[Store[s, _], ?, Y], q[Y]) => (s, Dir[q, Y]) => (s, Pos[q, Y])