package dynamical.fsm.methods.polymap

import dynamical.fsm.methods.types.{Init, Readout, Update}
import dynamical.fsm.Moore
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.{◁, ⊗}

object asMoore:

  extension [S, A, B, Y] (p: PolyMap[Monomial.Store[S, _], Monomial.Interface[A, B, _], Y])
    @scala.annotation.targetName("asMooreStoreToMono")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]] =
      new Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[A1, B1, A2, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBi")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]] =
      new Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _], Y] =
          p.`φ#`

  extension [S, A, B, Y] (p: PolyMap[PolyMap[Monomial.Store[S, _], Monomial.Interface[A, B, _], _], Monomial.Interface[A, A => B, _], Y])
    @scala.annotation.targetName("asMooreStoreToMonoToMono")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      new Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Monomial.Store[(S1, S2), _], Monomial.Interface[(A1, A2), (B1, B2), _], _ ], Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _], Y ])
    @scala.annotation.targetName("asMooreStoreToMonoToMonoTupled")
    def asMoore(i: (S1, S2)): Moore[Monomial.Store[(S1, S2), _] ~> Monomial.Interface[(A1, A2), (B1, B2), _] ~> Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]] =
      new Moore[Monomial.Store[(S1, S2), _] ~> Monomial.Interface[(A1, A2), (B1, B2), _] ~> Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]]:
        def init[Y]: Init[(Monomial.Store[(S1, S2), _]) ~> (Monomial.Interface[(A1, A2), (B1, B2), _]) ~> (Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[(S1, S2), _]) ~> (Monomial.Interface[(A1, A2), (B1, B2), _]) ~> (Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[(S1, S2), _]) ~> (Monomial.Interface[(A1, A2), (B1, B2), _]) ~> (Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Monomial.Store[S, _], Binomial.Interface[A1, B1, A2, B2, _], _], Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBiToBi")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      new Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], Monomial.Interface[A1, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono1")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _]] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _]]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _], Y] =
          p.`φ#`

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, I, O, Y] (p: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]),  Monomial.Interface[I, I => O, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreStoreTensorToMonoMonoMonoTensorToMono")
    def asMoore(i: ((S1, S2), S3)): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~>  Monomial.Interface[I, I => O, _]] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~>  Monomial.Interface[I, I => O, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], Monomial.Interface[A1, B1, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono2")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _]] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _]]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono3")
    def asMoore(i: (S1, S2)): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, A => C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono4")
    def asMoore(i: (S1, S2)): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.`φ#`    

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], Monomial.Interface[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _]] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToTensoredMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), _] , Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToBiBiTensorToBi")
    def asMoore(i: (S1, S2)): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Monomial.Store[S, _], (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, B2, _]), Y])
    @scala.annotation.targetName("asMooreMonomialStoreCompositeMonomialInterfaceMonomialInterface")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, B2, _])] =
      new Moore[Monomial.Store[S, _] ~> (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, B2, _])]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, B2, _]), Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> (Monomial.Interface[A1, B1, _] ◁ Monomial.Interface[A2, B2, _]), Y] =
          p.`φ#`
