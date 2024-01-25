package dynamical.fsm.methods.moore.product

import dynamical.fsm.methods.types.{Init, Readout, Update}
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.⊗

object tensor:

  extension [S1, S2, A1, B1, A2, B2, Y] (m1: Moore[Monomial.Store[S1, _] ~> Monomial.Interface[A1, B1, _]])
    @scala.annotation.targetName("tensorStoreMonoXStoreMono")
    def ⊗(
      m2: Moore[Monomial.Store[S2, _] ~> Monomial.Interface[A2, B2, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, Y] (m1: Moore[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])])
    @scala.annotation.targetName("tensorStoreMonoXStoreMonoXStoreMono")
    def ⊗(
      m2: Moore[Monomial.Store[S3, _] ~> Monomial.Interface[A3, B3, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _])] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _] ⊗ Monomial.Store[S3, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m1: Moore[Monomial.Store[S1, _] ~> Binomial.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("tensorStoreBiXStoreBi")
    def ⊗(
      m2: Moore[Monomial.Store[S2, _] ~> Binomial.Interface[A3, B3, A4, B4, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])] =
      new Moore[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Y] =
          (s => (m1.readout._1(s._1), m2.readout._1(s._2)), s => (m1.readout._2(s._1), m2.readout._2(s._2)))
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Y] =
          ((s, a) =>  (m1.update._1(s._1, a._1), s._2), (s, a) => (s._1, m2.update._2(s._2, a._2)))
