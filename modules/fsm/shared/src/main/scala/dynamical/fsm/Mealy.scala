package dynamical.fsm

import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Monomial, Binomial, Store}
import dynamical.fsm.{Init, Readout, Update}

trait Mealy[P[_]] extends Moore[P]:
  def run[Y]: Run[P, Y]

object Mealy:

  def apply[S, A, B, Y](
    i: S,
    r: S => A => B,
    u: (S, A) => S
  ): Mealy[Store[S, _] ~> Monomial[A, A => B, _]] =
    PolyMap[Store[S, _], Monomial[A, A => B, _], Y](r, u).asMealy(i)

  def apply[S, A, B, Y](
    i: S,
    r1: S => Some[A] => None.type,
    r2: S => None.type => Some[B],
    u1: (S, Some[A]) => S,
    u2: (S, None.type) => S
  ): Mealy[Store[S, _] ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]] =
    PolyMap[Store[S, _],  Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y]((r1, r2), (u1, u2))
      .asMealy(i)    

  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asMealyMono")
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

  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y])
    @scala.annotation.targetName("asMealyOptionOptionBi")
    def asMealy(i: S): Mealy[Store[S, _] ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]] =
      new Mealy[Store[S, _] ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]]:
        def init[Y]: Init[(Store[S, _]) ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y] =
          i
        def readout[Y]: Readout[(Store[S, _]) ~> (Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]), Y] =
          p.φ
        def update[Y]: Update[(Store[S, _]) ~> (Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]), Y] =
          (s, a) =>
            a match
              case Some(value) => (p.`φ#`._1(s, Some(value)), p.φ._1(s)(Some(value))) 
              case None => (p.`φ#`._2(s, None), p.φ._2(s)(None)) 

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