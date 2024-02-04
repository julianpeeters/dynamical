package dynamical.fsm.methods.moore

import cats.Id
import dynamical.fsm.{Moore, Wiring}
import dynamical.fsm.methods.polymap.asMoore.asMoore
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Bi, Mono}
import polynomial.product.⊗

object andThen:

  extension [S, A, B, Y] (m: Moore[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _]])
    @scala.annotation.targetName("andThenStoreMonoToMono")
    def andThen(
      w: Wiring[Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _]]
    ): Moore[Mono.Store[Id[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S, A, B, Y] (m: Moore[Mono.Store[Option[S], _] ~> Mono.Interface[A, Option[B], _]])
    @scala.annotation.targetName("andThenStoreMonoToMonoF")
    def andThen(
      w: Wiring[Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _]]
    ): Moore[Mono.Store[Option[S], _] ~> Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)
    
  extension [S, A1, B1, A2, B2, Y] (m: Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("andThenStoreBiToBi")
    def andThen(
      w: Wiring[Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _]]
    ): Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, A, B, C, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono0")
    def andThen(
      w: Wiring[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono1")
    def andThen(
      w: Wiring[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
                                        
  extension [S1, S2, A1, B1, A2, B2, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono2")
    def andThen(
      w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, B2, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, B2, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoRunnable")
    def andThen(
      w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~>
      (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~>
        Mono.Interface[A1, A1 => B2, _]
    ] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoMonoTensor")
    def andThen(
      w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])]
    ): Moore[
      (Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~>
        (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~>
          (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])
    ] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _])])
    @scala.annotation.targetName("andThenStoreStoreStoreTensorMonoMonoMnooTensorToMono")
    def andThen[I, O](
      w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~>  Mono.Interface[I, I => O, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorBiBiTensorToBi")
    def andThen(
      w: Wiring[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~>  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]]
    )
    : Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~>  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)
