package dynamical.fsm.methods.moore.product

import dynamical.fsm.methods.types.{Init, Readout, Update}
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.{Bi, Mono}
import polynomial.product.⊗

object tensor:

  extension [S1, S2, A1, B1, A2, B2, Y] (m1: Moore[Mono.Store[S1, _] ~> Mono.Interface[A1, B1, _]])
    @scala.annotation.targetName("tensorStoreMonoXStoreMono")
    def ⊗(
      m2: Moore[Mono.Store[S2, _] ~> Mono.Interface[A2, B2, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _])] =
      new Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _])]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, Y] (m1: Moore[(Mono.Store[S1, _]) ⊗ (Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _])])
    @scala.annotation.targetName("tensorStoreMonoXStoreMonoXStoreMono")
    def ⊗(
      m2: Moore[Mono.Store[S3, _] ~> Mono.Interface[A3, B3, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _])] =
      new Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _])]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m1: Moore[Mono.Store[S1, _] ~> Bi.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("tensorStoreBiXStoreBi")
    def ⊗(
      m2: Moore[Mono.Store[S2, _] ~> Bi.Interface[A3, B3, A4, B4, _]]
    ): Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])] =
      new Moore[(Mono.Store[S1, _]) ⊗ (Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])]:
        def init[Y]: Init[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]), Y] =
          (s => (m1.readout._1(s._1), m2.readout._1(s._2)), s => (m1.readout._2(s._1), m2.readout._2(s._2)))
        def update[Y]: Update[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]), Y] =
          ((s, a) =>  (m1.update._1(s._1, a._1), s._2), (s, a) => (s._1, m2.update._2(s._2, a._2)))
