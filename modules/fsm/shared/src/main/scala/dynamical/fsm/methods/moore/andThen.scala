package dynamical.fsm.methods.moore

import cats.{Id, Monad}
import dynamical.fsm.{Moore, Wiring}
import dynamical.fsm.methods.polymap.asMoore.asMoore
import dynamical.fsm.methods.wiring.asPolyMap.asPolyMap
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store, StoreF}
import polynomial.product.{×, ⊗}

object andThen:

  extension [S, A, B, C, D] (m: Moore[Store[Id[S], _] ~> Interface[A, Id[B], _]])
    @scala.annotation.targetName("andThenStoreMonoToMono")
    def andThen(
      w: Wiring[Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _]]
    ): Moore[Store[Id[S], _] ~> Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S, A, B] (m: Moore[Store[Option[S], _] ~> Interface[A, Option[B], _]])
    @scala.annotation.targetName("andThenStoreMonoToMonoF")
    def andThen(
      w: Wiring[Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _]]
    ): Moore[Store[Option[S], _] ~> Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [F[_]: Monad, S, A, B] (m: Moore[StoreF[F, S, _] ~> Interface[A, F[B], _]])
    @scala.annotation.targetName("andThenFStoreMonoToMonoF")
    def andThen(
      w: Wiring[Interface[A, F[B], _] ~> Interface[A, A => F[B], _]]
    ): Moore[StoreF[F, S, _] ~> Interface[A, F[B], _] ~> Interface[A, A => F[B], _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)
    
  extension [S, A1, B1, A2, B2] (m: Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("andThenStoreBiToBi")
    def andThen(
      w: Wiring[BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]]
    ): Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, A, B, C] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono0")
    def andThen(
      w: Wiring[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _]]
    ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono1")
    def andThen(
      w: Wiring[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]]
    ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
                                        
  extension [S1, S2, A1, B1, A2, B2] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono2")
    def andThen(
      w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, B2, _]]
    ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, B2, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoRunnable")
    def andThen(
      w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]]
    ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~>
      (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~>
        Interface[A1, A1 => B2, _]
    ] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoMonoTensor")
    def andThen(
      w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])]
    ): Moore[
      (Store[S1, _] ⊗ Store[S2, _]) ~>
        (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~>
          (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])
    ] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3] (m: Moore[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _])])
    @scala.annotation.targetName("andThenStoreStoreStoreTensorMonoMonoMnooTensorToMono")
    def andThen[I, O](
      w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~>  Interface[I, I => O, _]]
    ): Moore[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorBiBiTensorToBi")
    def andThen(
      w: Wiring[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>  BiInterface[A1, A1 => B3, A2, A2 => B4, _]]
    )
    : Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>  BiInterface[A1, A1 => B3, A2, A2 => B4, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)
             
  extension [S, A1, B1, A2, B2] (m: Moore[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _])])
    @scala.annotation.targetName("andThenStoreCartesianMonoMono")
    def andThen(
      w: Wiring[(Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]]
    ): Moore[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]] =
       m.asPolyMap
        .andThen(w.asPolyMap)
        .asMoore(m.init)

  extension [S, A1, B1, A2, B2, A3, B3] (m: Moore[Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])])
    @scala.annotation.targetName("andThenStoreCartesianMonoTensoredMono")
    def andThen(
      w: Wiring[((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]) ~> Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]]
    ): Moore[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]] =
       m.asPolyMap
        .andThen(w.asPolyMap)
        .asMoore(m.init)

  extension [S, A1, B1, A2, B2, A3, B3] (m: Moore[Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))])
    @scala.annotation.targetName("andThenStoreTensoredMonoCartesianMono")
    def andThen(
      w: Wiring[(Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _]]
    ): Moore[(Store[Id[S], _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _]] =
       m.asPolyMap
        .andThen(w.asPolyMap)
        .asMoore(m.init)

  extension [S, A1, B1, A2, B2, A3, B3] (m: Moore[Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])])
    @scala.annotation.targetName("andThenStoreCartesianMonoTensoredMonoToTensoredMonoCartesianMono")
    def andThen(
      w: Wiring[((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))]
    ): Moore[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))] =
       m.asPolyMap
        .andThen(w.asPolyMap)
        .asMoore(m.init)

  extension [S, A1, B1, A2, B2, A3, B3] (m: Moore[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))])
    @scala.annotation.targetName("andThenStoreCartesianMonoTensoredMonoToTensoredMonoCartesianMonoToMono")
    def andThen(
      w: Wiring[(Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _]]
    ): Moore[((Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _]] =
       m.asPolyMap
        .andThen(w.asPolyMap)
        .asMoore(m.init)  