package dynamical.fsm.methods.polymap

import dynamical.fsm.methods.types.{Init, Readout, Run, Update}
import dynamical.fsm.methods.mealy.run.Runner2
import dynamical.fsm.Mealy
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}

object asMealy:

  extension [S, A, B, Y] (p: PolyMap[Monomial.Store[S, _], Monomial.Interface[A, A => B, _], Y])
    @scala.annotation.targetName("asMealyStoreToMono")
    def asMealy(i: S): Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]] =
      new Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[(Monomial.Store[S, _]) ~> Monomial.Interface[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, A => B, _]), Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, A => B, _]), Y] =
          (s: S, a: A) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, B, Y] (p: (Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _])[Y])
    @scala.annotation.targetName("asMealyStoreToMonoToMono")
    def asMealy(i: S): Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      new Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]), Y] =
          (s, a) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, A1, B1, A2, B2, Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y])
    def asMealy(i: S)(
      using R: Runner2[Monomial.Store[S, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], S, A1, B1, A2, B2]
    ): Mealy[Monomial.Store[S, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
    new Mealy[Monomial.Store[S, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]]:
      def init[Y]: Init[(Monomial.Store[S, _]) ~> (Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        i
      def readout[Y]: Readout[(Monomial.Store[S, _]) ~> (Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.φ
      def update[Y]: Update[(Monomial.Store[S, _]) ~> (Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.`φ#`
      def run[Y]: Run[(Monomial.Store[S, _]) ~> (Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.run