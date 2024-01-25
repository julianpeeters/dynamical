package dynamical.fsm.methods.moore

import cats.evidence.Is
import dynamical.fsm.{Moore, Wiring}
import dynamical.fsm.methods.polymap.asMoore.asMoore
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.⊗

object andThen:

  extension [S, A, B, Y] (m: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]])
    @scala.annotation.targetName("andThenStoreMonoToMono")
    def andThen(
      w: Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]
    ): Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("andThenStoreBiToBi")
    def andThen(
      w: Wiring[Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]]
    ): Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, A, B, C, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono0")
    def andThen(
      w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono1")
    def andThen(
      w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
                                        
  extension [S1, S2, A1, B1, A2, B2, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono2")
    def andThen(
      w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B2, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B2, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoRunnable")
    def andThen(
      w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~>
      (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~>
        Monomial.Interface[A1, A1 => B2, _]
    ] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoMonoTensor")
    def andThen(
      w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])]
    ): Moore[
      (Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~>
        (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~>
          (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])
    ] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorBiBiTensorToBi")
    def andThen(
      w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~>  Monomial.Interface[Unit, Unit => Unit, _]]
    )(using
      F: Is[S1, A1],
      G: Is[S2, A2],
      H: Is[S3, A3],
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[Unit, Unit => Unit, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap, F.coerce, G.coerce, H.coerce)
       .asMoore(m.init)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorBiBiTensorToBi")
    def andThen(
      w: Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]]
    )
    : Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)
