package dynamical.fsm.methods.polymap

import cats.Id
import dynamical.fsm.methods.types.{Init, Readout, Update}
import dynamical.fsm.Moore
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store, StoreF}
import polynomial.product.{×, ◁, ⊗}

object asMoore:

  extension [S, A, B, Y] (p: PolyMap[Store[Id[S], _], Interface[A, Id[B], _], Y])
    @scala.annotation.targetName("asMooreStoreToMono")
    def asMoore(i: S): Moore[Store[Id[S], _] ~> Interface[A, Id[B], _]] =
      new Moore[Store[S, _] ~> Interface[A, Id[B], _]]:
        def init[Y]: Init[Store[S, _] ~> Interface[A, Id[B], _], Y] =
          i
        def readout[Y]: Readout[Store[Id[S], _] ~> Interface[A, Id[B], _], Y] =
          p.φ
        def update[Y]: Update[Store[Id[S], _] ~> Interface[A, Id[B], _], Y] =
          p.`φ#`

  extension [F[_], S, A, B, Y] (p: PolyMap[StoreF[F, S, _], Interface[A, F[B], _], Y])
    @scala.annotation.targetName("asMooreFStoreToMono")
    def asMoore(i: S): Moore[StoreF[F, S, _] ~> Interface[A, F[B], _]] =
      new Moore[StoreF[F, S, _] ~> Interface[A, F[B], _]]:
        def init[Y]: Init[StoreF[F, S, _] ~> Interface[A, F[B], _], Y] =
          i
        def readout[Y]: Readout[StoreF[F, S, _] ~> Interface[A, F[B], _], Y] =
          p.φ
        def update[Y]: Update[StoreF[F, S, _] ~> Interface[A, F[B], _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Store[S, _], BiInterface[A1, B1, A2, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBi")
    def asMoore(i: S): Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _]] =
      new Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _]]:
        def init[Y]: Init[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _], Y] =
          p.`φ#`

  extension [S, A, B, C, D, Y] (p: PolyMap[PolyMap[Store[Id[S], _], Interface[A, Id[B], _], _], Interface[C, C => Id[D], _], Y])
    @scala.annotation.targetName("asMooreStoreToMonoToMono")
    def asMoore(i: S): Moore[Store[Id[S], _] ~> Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _]] =
      new Moore[Store[Id[S], _] ~> Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _]]:
        def init[Y]: Init[Store[Id[S], _] ~> Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _], Y] =
          i
        def readout[Y]: Readout[Store[Id[S], _] ~> Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _], Y] =
          p.φ
        def update[Y]: Update[Store[Id[S], _] ~> Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _], Y] =
          p.`φ#`

  extension [S, A, B, Y] (p: PolyMap[PolyMap[Store[Option[S], _], Interface[A, Id[B], _], _], Interface[A, A => Option[B], _], Y])
    @scala.annotation.targetName("asMooreStoreToMonoToMonoPrism")
    def asMoore(i: Option[S]): Moore[Store[Option[S], _] ~> Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _]] =
      new Moore[Store[Option[S], _] ~> Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _]]:
        def init[Y]: Init[Store[Option[S], _] ~> Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _], Y] =
          i
        def readout[Y]: Readout[Store[Option[S], _] ~> Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _], Y] =
          p.φ
        def update[Y]: Update[Store[Option[S], _] ~> Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _], Y] =
          p.`φ#`

  extension [F[_], S, A, B, Y] (p: PolyMap[PolyMap[StoreF[F, S, _], Interface[A, F[B], _], _], Interface[A, A => F[B], _], Y])
    @scala.annotation.targetName("asMooreStoreToMonoToMonoPrism")
    def asMoore(i: S): Moore[StoreF[F, S, _] ~> Interface[A, F[B], _] ~> Interface[A, A => F[B], _]] =
      new Moore[(StoreF[F, S, _] ~> Interface[A, F[B], _]) ~> Interface[A, A => F[B], _]]:
        def init[Y]: Init[(StoreF[F, S, _] ~> Interface[A, F[B], _]) ~> Interface[A, A => F[B], _], Y] =
          i
        def readout[Y]: Readout[(StoreF[F, S, _] ~> Interface[A, F[B], _]) ~> Interface[A, A => F[B], _], Y] =
          p.φ
        def update[Y]: Update[(StoreF[F, S, _] ~> Interface[A, F[B], _]) ~> Interface[A, A => F[B], _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Store[(S1, S2), _], Interface[(A1, A2), (B1, B2), _], _ ], Interface[(A1, A2), (A1 => B1, A2 => B2), _], Y ])
    @scala.annotation.targetName("asMooreStoreToMonoToMonoTupled")
    def asMoore(i: (S1, S2)): Moore[Store[(S1, S2), _] ~> Interface[(A1, A2), (B1, B2), _] ~> Interface[(A1, A2), (A1 => B1, A2 => B2), _]] =
      new Moore[Store[(S1, S2), _] ~> Interface[(A1, A2), (B1, B2), _] ~> Interface[(A1, A2), (A1 => B1, A2 => B2), _]]:
        def init[Y]: Init[(Store[(S1, S2), _]) ~> (Interface[(A1, A2), (B1, B2), _]) ~> (Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          i
        def readout[Y]: Readout[(Store[(S1, S2), _]) ~> (Interface[(A1, A2), (B1, B2), _]) ~> (Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.φ
        def update[Y]: Update[(Store[(S1, S2), _]) ~> (Interface[(A1, A2), (B1, B2), _]) ~> (Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Store[S, _], BiInterface[A1, B1, A2, B2, _], _], BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBiToBi")
    def asMoore(i: S): Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]] =
      new Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]]:
        def init[Y]: Init[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _], Interface[A1, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono1")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B2, _]] =
      new Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B2, _]]:
        def init[Y]: Init[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B2, _], Y] =
          p.`φ#`

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, I, O, Y] (p: PolyMap[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]),  Interface[I, I => O, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreStoreTensorToMonoMonoMonoTensorToMono")
    def asMoore(i: ((S1, S2), S3)): Moore[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~>  Interface[I, I => O, _]] =
      new Moore[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~>  Interface[I, I => O, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          i
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          p.φ
        def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _], Interface[A1, B1, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono2")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B1, _]] =
      new Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B1, _]]:
        def init[Y]: Init[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B1, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B1, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, B1, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]), Interface[A, C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono3")
    def asMoore(i: (S1, S2)): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _]] =
      new Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _], Y] =
          i
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _], Y] =
          p.φ
        def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]), Interface[A, A => C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono4")
    def asMoore(i: (S1, S2)): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]] =
      new Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          i
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          p.φ
        def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          p.`φ#`    

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _], Interface[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, A1 => B2, _]] =
      new Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, A1 => B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, A1 => B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> Interface[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _], (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToTensoredMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])] =
      new Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          i
        def readout[Y]: Readout[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), _] ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]), _] , BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToBiBiTensorToBi")
    def asMoore(i: (S1, S2)): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          i
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Store[S, _], (Interface[A1, B1, _] × Interface[A2, B2, _]), Y])
    @scala.annotation.targetName("asMooreMonoStoreCartesianMonoInterface")
    def asMoore(i: S): Moore[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _])] =
      new Moore[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _])]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]), Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: PolyMap[Store[S, _], ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), Y])
    @scala.annotation.targetName("asMooreMonoStoreCartesianTensorMonoInterface")
    def asMoore(i: S): Moore[Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])] =
      new Moore[Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: PolyMap[PolyMap[Store[S, _], ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), _], Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y])
    @scala.annotation.targetName("asMooreMonoStoreCartesianTensorMonoInterfaceMealified")
    def asMoore(i: S): Moore[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]] =
      new Moore[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y] =
          p.φ
        def update[Y]: Update[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: PolyMap[Store[Id[S], _], (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y])
    @scala.annotation.targetName("asMooreMonoStoreTensorCartesianMonoInterface")
    def asMoore(i: S): Moore[Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))] =
      new Moore[Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y] =
          p.φ
        def update[Y]: Update[Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: PolyMap[PolyMap[Store[Id[S], _], (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), _], Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _], Y])
    @scala.annotation.targetName("asMooreMonoStoreTensorCartesianMonoInterfaceMealified")
    def asMoore(i: S): Moore[(Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _]] =
      new Moore[(Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _]]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[(Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _], Y] =
          p.φ
        def update[Y]: Update[(Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: PolyMap[PolyMap[Store[Id[S], _], ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), _], (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y])
    @scala.annotation.targetName("asMooreMonoStoreCartesianTensorMonoInterfaceTensorCartesianMono")
    def asMoore(i: S): Moore[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))] =
      new Moore[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y] =
          p.φ
        def update[Y]: Update[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: PolyMap[PolyMap[PolyMap[Store[S, _], ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), _], (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), _], Interface[Either[A1, (A2, A3)], (Either[A1, (A2, A3)]) => (B1, (B2, B3)), _], Y])
    @scala.annotation.targetName("asMooreMonoStoreCartesianTensorMonoInterfaceTensorCartesianMonoDistributive")
    def asMoore(i: S): Moore[((Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], (Either[A1, (A2, A3)]) => (B1, (B2, B3)), _]] =
      new Moore[((Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], (Either[A1, (A2, A3)]) => (B1, (B2, B3)), _]]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[((Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], (Either[A1, (A2, A3)]) => (B1, (B2, B3)), _], Y] =
          p.φ
        def update[Y]: Update[((Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], (Either[A1, (A2, A3)]) => (B1, (B2, B3)), _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Store[S, _], (Interface[A1, B1, _] × Interface[A2, B2, _]), _], Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y])
    @scala.annotation.targetName("asMooreMonoStoreCartesianMonoInterfaceMonoInterface")
    def asMoore(i: S): Moore[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]] =
      new Moore[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Store[S, _], (Interface[A1, B1, _] ◁ Interface[A2, B2, _]), Y])
    @scala.annotation.targetName("asMooreMonoStoreCompositeMonoInterfaceMonoInterface")
    def asMoore(i: S): Moore[Store[S, _] ~> (Interface[A1, B1, _] ◁ Interface[A2, B2, _])] =
      new Moore[Store[S, _] ~> (Interface[A1, B1, _] ◁ Interface[A2, B2, _])]:
        def init[Y]: S =
          i
        def readout[Y]: Readout[Store[S, _] ~> (Interface[A1, B1, _] ◁ Interface[A2, B2, _]), Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> (Interface[A1, B1, _] ◁ Interface[A2, B2, _]), Y] =
          p.`φ#`
