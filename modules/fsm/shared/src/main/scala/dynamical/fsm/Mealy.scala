package dynamical.fsm

import dynamical.fsm.internal.{Init, Readout, Run, Update}
import dynamical.fsm.run.Runner2
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial, Store}
import polynomial.product.⊗

trait Mealy[P[_]] extends Moore[P]:
  def run[Y]: Run[P, Y]

object Mealy:

  def apply[S, A, B, Y](
    i: S,
    r: S => A => B,
    u: (S, A) => S
  ): Mealy[Store[S, _] ~> Monomial[A, A => B, _]] =
    PolyMap[Store[S, _], Monomial[A, A => B, _], Y](r, u).asMealy(i)

  def apply[S, A1, B1, A2, B2, Y](
    i: S,
    r1: S => A1 => B1,
    r2: S => A2 => B2,
    u1: (S, A1) => S,
    u2: (S, A2) => S
  )(
    using Runner2[Store[S, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], S, A1, B1, A2, B2]
  ): Mealy[Store[S, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
    PolyMap[Store[S, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y]((r1, r2), (u1, u2)).asMealy(i)

  extension [S1, S2, A1, B1, A2, B2, Y] (m1: Mealy[Store[S1, _] ~> Monomial[A1, A1 => B1, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(m2: Mealy[Store[S2, _] ~> Monomial[A2, A2 => B2, _]]): Mealy[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])] =
      new Mealy[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])]:
        def init[Y]: Init[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))
        def run[Y]: Run[(Store[S1, _]) ⊗ (Store[S2, _]) ~> ((Monomial[A1, A1 => B1, _]) ⊗ (Monomial[A2, A2 => B2, _])), Y] =
          (s, a) => (update(s, a), (readout(s)._1(a._1), readout(s)._2(a._2)))
      
  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asMealyStoreToMono")
    def asMealy(i: S): Mealy[Store[S, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[(Store[S, _]) ~> Monomial[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          p.φ
        def update[Y]: Update[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          (s: S, a: A) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, B, Y] (p: (Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _])[Y])
    @scala.annotation.targetName("asMealyStoreToMonoToMono")
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

  extension [S, A, A1, B1, A2, B2, Y] (p: PolyMap[Store[S, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y])
    def asMealy(i: S)(
      using R: Runner2[Store[S, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], S, A1, B1, A2, B2]
    ): Mealy[Store[S, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
    new Mealy[Store[S, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]]:
      def init[Y]: Init[(Store[S, _]) ~> (Binomial[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        i
      def readout[Y]: Readout[(Store[S, _]) ~> (Binomial[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.φ
      def update[Y]: Update[(Store[S, _]) ~> (Binomial[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.`φ#`
      def run[Y]: Run[(Store[S, _]) ~> (Binomial[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.run