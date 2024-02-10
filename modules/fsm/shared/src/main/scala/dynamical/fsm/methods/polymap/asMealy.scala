package dynamical.fsm.methods.polymap

import dynamical.fsm.methods.types.{Init, Readout, Run, Update}
import dynamical.fsm.methods.mealy.run.Runner2
import dynamical.fsm.Mealy
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store}

object asMealy:

  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Interface[A, A => B, _], Y])
    @scala.annotation.targetName("asMealyStoreToMono")
    def asMealy(i: S): Mealy[Store[S, _] ~> Interface[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Interface[A, A => B, _]]:
        def init[Y]: Init[(Store[S, _]) ~> Interface[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[(Store[S, _]) ~> (Interface[A, A => B, _]), Y] =
          p.φ
        def update[Y]: Update[(Store[S, _]) ~> (Interface[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Interface[A, A => B, _]), Y] =
          (s: S, a: A) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, B, Y] (p: (Store[S, _] ~> Interface[A, B, _] ~> Interface[A, A => B, _])[Y])
    @scala.annotation.targetName("asMealyStoreToMonoToMono")
    def asMealy(i: S): Mealy[Store[S, _] ~> Interface[A, B, _] ~> Interface[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Interface[A, B, _] ~> Interface[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Interface[A, B, _] ~> Interface[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Interface[A, B, _] ~> Interface[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[(Store[S, _] ~> Interface[A, B, _] ~> Interface[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Interface[A, B, _] ~> Interface[A, A => B, _]), Y] =
          (s, a) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, A1, B1, A2, B2, Y] (p: PolyMap[Store[S, _], BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y])
    def asMealy(i: S)(
      using R: Runner2[Store[S, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _], S, A1, B1, A2, B2]
    ): Mealy[Store[S, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]] =
    new Mealy[Store[S, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]]:
      def init[Y]: Init[(Store[S, _]) ~> (BiInterface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        i
      def readout[Y]: Readout[(Store[S, _]) ~> (BiInterface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.φ
      def update[Y]: Update[(Store[S, _]) ~> (BiInterface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.`φ#`
      def run[Y]: Run[(Store[S, _]) ~> (BiInterface[A1, A1 => B1, A2, A2 => B2, _]), Y] =
        p.run