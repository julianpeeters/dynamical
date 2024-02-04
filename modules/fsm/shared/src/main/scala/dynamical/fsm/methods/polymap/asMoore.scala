package dynamical.fsm.methods.polymap

import cats.Id
import dynamical.fsm.methods.types.{Init, Readout, Update}
import dynamical.fsm.Moore
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Bi, Mono}
import polynomial.product.{◁, ⊗}

object asMoore:

  extension [S, A, B, Y] (p: PolyMap[Mono.Store[Id[S], _], Mono.Interface[A, Id[B], _], Y])
    @scala.annotation.targetName("asMooreStoreToMono")
    def asMoore(i: S): Moore[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _]] =
      new Moore[Mono.Store[S, _] ~> Mono.Interface[A, Id[B], _]]:
        def init[Y]: Init[Mono.Store[S, _] ~> Mono.Interface[A, Id[B], _], Y] =
          i
        def readout[Y]: Readout[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _], Y] =
          p.φ
        def update[Y]: Update[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Mono.Store[S, _], Bi.Interface[A1, B1, A2, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBi")
    def asMoore(i: S): Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _]] =
      new Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _]]:
        def init[Y]: Init[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _], Y] =
          i
        def readout[Y]: Readout[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _], Y] =
          p.φ
        def update[Y]: Update[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _], Y] =
          p.`φ#`

  extension [S, A, B, Y] (p: PolyMap[PolyMap[Mono.Store[Id[S], _], Mono.Interface[A, Id[B], _], _], Mono.Interface[A, A => Id[B], _], Y])
    @scala.annotation.targetName("asMooreStoreToMonoToMono")
    def asMoore(i: S): Moore[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _]] =
      new Moore[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _]]:
        def init[Y]: Init[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _], Y] =
          i
        def readout[Y]: Readout[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _], Y] =
          p.φ
        def update[Y]: Update[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _], Y] =
          p.`φ#`

  
  extension [S, A, B, Y] (p: PolyMap[PolyMap[Mono.Store[Option[S], _], Mono.Interface[A, Id[B], _], _], Mono.Interface[A, A => Option[B], _], Y])
    @scala.annotation.targetName("asMooreStoreToMonoToMonoPrism")
    def asMoore(i: Option[S]): Moore[Mono.Store[Option[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _]] =
      new Moore[Mono.Store[Option[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _]]:
        def init[Y]: Init[Mono.Store[Option[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _], Y] =
          i
        def readout[Y]: Readout[Mono.Store[Option[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _], Y] =
          p.φ
        def update[Y]: Update[Mono.Store[Option[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Mono.Store[(S1, S2), _], Mono.Interface[(A1, A2), (B1, B2), _], _ ], Mono.Interface[(A1, A2), (A1 => B1, A2 => B2), _], Y ])
    @scala.annotation.targetName("asMooreStoreToMonoToMonoTupled")
    def asMoore(i: (S1, S2)): Moore[Mono.Store[(S1, S2), _] ~> Mono.Interface[(A1, A2), (B1, B2), _] ~> Mono.Interface[(A1, A2), (A1 => B1, A2 => B2), _]] =
      new Moore[Mono.Store[(S1, S2), _] ~> Mono.Interface[(A1, A2), (B1, B2), _] ~> Mono.Interface[(A1, A2), (A1 => B1, A2 => B2), _]]:
        def init[Y]: Init[(Mono.Store[(S1, S2), _]) ~> (Mono.Interface[(A1, A2), (B1, B2), _]) ~> (Mono.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          i
        def readout[Y]: Readout[(Mono.Store[(S1, S2), _]) ~> (Mono.Interface[(A1, A2), (B1, B2), _]) ~> (Mono.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.φ
        def update[Y]: Update[(Mono.Store[(S1, S2), _]) ~> (Mono.Interface[(A1, A2), (B1, B2), _]) ~> (Mono.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Mono.Store[S, _], Bi.Interface[A1, B1, A2, B2, _], _], Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBiToBi")
    def asMoore(i: S): Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      new Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _]]:
        def init[Y]: Init[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          i
        def readout[Y]: Readout[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def update[Y]: Update[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _], Mono.Interface[A1, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono1")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B2, _]] =
      new Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B2, _]]:
        def init[Y]: Init[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B2, _], Y] =
          p.`φ#`

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, I, O, Y] (p: PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]),  Mono.Interface[I, I => O, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreStoreTensorToMonoMonoMonoTensorToMono")
    def asMoore(i: ((S1, S2), S3)): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~>  Mono.Interface[I, I => O, _]] =
      new Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~>  Mono.Interface[I, I => O, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          i
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          p.φ
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _], Mono.Interface[A1, B1, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono2")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B1, _]] =
      new Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B1, _]]:
        def init[Y]: Init[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B1, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B1, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, B1, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]), Mono.Interface[A, C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono3")
    def asMoore(i: (S1, S2)): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _]] =
      new Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _], Y] =
          i
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _], Y] =
          p.φ
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]), Mono.Interface[A, A => C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono4")
    def asMoore(i: (S1, S2)): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]] =
      new Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          i
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          p.φ
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          p.`φ#`    

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _], Mono.Interface[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, A1 => B2, _]] =
      new Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, A1 => B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, A1 => B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> Mono.Interface[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _], (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _]), Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToTensoredMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])] =
      new Moore[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _]), Y] =
          i
        def readout[Y]: Readout[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _]), Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), _] ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _]), Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]), _] , Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToBiBiTensorToBi")
    def asMoore(i: (S1, S2)): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          i
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Mono.Store[S, _], (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, B2, _]), Y])
    @scala.annotation.targetName("asMooreMonoStoreCompositeMonoInterfaceMonoInterface")
    def asMoore(i: S): Moore[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, B2, _])] =
      new Moore[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, B2, _])]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, B2, _]), Y] =
          p.φ
        def update[Y]: Update[Mono.Store[S, _] ~> (Mono.Interface[A1, B1, _] ◁ Mono.Interface[A2, B2, _]), Y] =
          p.`φ#`
