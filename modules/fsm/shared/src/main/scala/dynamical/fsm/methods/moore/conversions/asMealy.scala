package dynamical.fsm.methods.moore.conversions

import dynamical.fsm.methods.types.{Init, Readout, Run, Update}
import dynamical.fsm.{Mealy, Moore}
import polynomial.morphism.~>
import polynomial.`object`.{Bi, Mono}
import polynomial.product.{⊗, ◁}
import dynamical.fsm.methods.mealy.run.Runner2

object asMealy:
    
  extension [S, A, B, Y] (p: Moore[Mono.Store[S, _] ~> Mono.Interface[A, A => B, _]])
    @scala.annotation.targetName("asMealyStoreToMono")
    def asMealy: Moore[Mono.Store[S, _] ~> Mono.Interface[A, A => B, _]] =
      new Mealy[Mono.Store[S, _] ~> Mono.Interface[A, A => B, _]]:
        def init[Y]: Init[Mono.Store[S, _] ~> Mono.Interface[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Mono.Store[S, _] ~> Mono.Interface[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[Mono.Store[S, _] ~> Mono.Interface[A, A => B, _], Y] =
          p.update
        def run[Y]: Run[(Mono.Store[S, _]) ~> (Mono.Interface[A, A => B, _]), Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S, A, B, Y] (p: Moore[Mono.Store[S, _] ~> Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _]])
    @scala.annotation.targetName("asMealyStoreToMonoToMono")
    def asMealy: Mealy[Mono.Store[S, _] ~> Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _]] =
      new Mealy[Mono.Store[S, _] ~> Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _]]:
        def init[Y]: Init[Mono.Store[S, _] ~> Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Mono.Store[S, _] ~> Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[(Mono.Store[S, _] ~> Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _]), Y] =
          p.update
        def run[Y]: Run[(Mono.Store[S, _]) ~> (Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _]), Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A, B, C, Y] (p: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono2")
    def asMealy: Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]] =
      new Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          p.init
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          p.readout
        def update[Y]: Update[((Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]), Y] =
          p.update
        def run[Y]: Run[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          (s: (S1, S2), a: A) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono")
    def asMealy: Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]] =
      new Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _], Y] =
          p.init
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _], Y] =
          p.readout
        def update[Y]: Update[((Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]), Y] =
          p.update
        def run[Y]: Run[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _], Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))
        
  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMonoMonoTensor")
    def asMealy: Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])] =
      new Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _]), Y] =
          p.init
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _]), Y] =
          p.readout
        def update[Y]: Update[((Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])), Y] =
          p.update
        def run[Y]: Run[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _]), Y] =
          (s, a) => (p.update(s, a), (p.readout(s)._1(a._1), p.readout(s)._2(a._2)))
        
  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, I, O, Y] (p: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _]])
    def asMealy: Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _]] =
      new Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          p.init
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          p.readout
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          p.update
        def run[Y]: Run[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~>  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]] )
    def asMealy(using R: Runner2[((Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~>  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]), (S1, S2), A1, B3, A2, B4]): Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~>  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Mealy[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~>  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> ((Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])) ~> (Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.init
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> ((Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])) ~> (Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.readout
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> ((Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])) ~> (Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.update
        def run[Y]: Run[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> ((Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])) ~> (Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
         p.asPolyMap.run

  extension [S, A1, B1, A2, B2, Y](m: Moore[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asMealyStoreToMonoCompositeMono")
    def asMealy: Mealy[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, A2 => B2, _])] =
      new Mealy[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Mono.Store[S, _]) ~> ((Mono.Interface[A1, B1, _]) ◁ (Mono.Interface[A2, A2 => B2, _])), Y] =
          m.init
        def readout[Y]: Readout[(Mono.Store[S, _]) ~> ((Mono.Interface[A1, B1, _]) ◁ (Mono.Interface[A2, A2 => B2, _])), Y] =
          m.readout
        def update[Y]: Update[(Mono.Store[S, _]) ~> ((Mono.Interface[A1, B1, _]) ◁ (Mono.Interface[A2, A2 => B2, _])), Y] =
          m.update
        def run[Y]: Run[(Mono.Store[S, _]) ~> ((Mono.Interface[A1, B1, _]) ◁ (Mono.Interface[A2, A2 => B2, _])), Y] =
          (s, a) => (m.update(s, a), (m.readout._1(s), m.readout._2(m.readout._1(s))(a._2)))
