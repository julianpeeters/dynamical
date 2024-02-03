package dynamical.fsm.methods.moore.conversions

import dynamical.fsm.methods.types.{Init, Readout, Run, Update}
import dynamical.fsm.{Mealy, Moore}
import polynomial.morphism.~>
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.{⊗, ◁}
import dynamical.fsm.methods.mealy.run.Runner2

object asMealy:
    
  extension [S, A, B, Y] (p: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]])
    @scala.annotation.targetName("asMealyStoreToMono")
    def asMealy: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]] =
      new Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, A => B, _]), Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S, A, B, Y] (p: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]])
    @scala.annotation.targetName("asMealyStoreToMonoToMono")
    def asMealy: Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      new Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[(Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]), Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A, B, C, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono2")
    def asMealy: Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.readout
        def update[Y]: Update[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          (s: (S1, S2), a: A) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono")
    def asMealy: Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.readout
        def update[Y]: Update[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))
        
  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMonoMonoTensor")
    def asMealy: Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.readout
        def update[Y]: Update[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          (s, a) => (p.update(s, a), (p.readout(s)._1(a._1), p.readout(s)._2(a._2)))
        
  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, I, O, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _]])
    def asMealy: Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _]] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          p.readout
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] )
    def asMealy(using R: Runner2[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), (S1, S2), A1, B3, A2, B4]): Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.readout
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
         p.asPolyMap.run

  extension [S, A1, B1, A2, B2, Y](m: Moore[Monomial.Store[S, _] ~> (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asMealyStoreToMonoCompositeMono")
    def asMealy: Mealy[Monomial.Store[S, _] ~> (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, A2 => B2, _])] =
      new Mealy[Monomial.Store[S, _] ~> (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Monomial.Store[S, _]) ~> ((Monomial.Interface[A1, B1, _]) ◁ (Monomial.Interface[A2, A2 => B2, _])), Y] =
          m.init
        def readout[Y]: Readout[(Monomial.Store[S, _]) ~> ((Monomial.Interface[A1, B1, _]) ◁ (Monomial.Interface[A2, A2 => B2, _])), Y] =
          m.readout
        def update[Y]: Update[(Monomial.Store[S, _]) ~> ((Monomial.Interface[A1, B1, _]) ◁ (Monomial.Interface[A2, A2 => B2, _])), Y] =
          m.update
        def run[Y]: Run[(Monomial.Store[S, _]) ~> ((Monomial.Interface[A1, B1, _]) ◁ (Monomial.Interface[A2, A2 => B2, _])), Y] =
          (s, a) => (m.update(s, a), (m.readout._1(s), m.readout._2(m.readout._1(s))(a._2)))
