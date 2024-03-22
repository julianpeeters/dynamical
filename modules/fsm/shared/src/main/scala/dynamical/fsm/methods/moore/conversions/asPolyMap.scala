package dynamical.fsm.methods.moore.conversions

import cats.Id
import dynamical.fsm.Moore
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store, StoreF}
import polynomial.product.{×, ⊗}

object asPolyMap:

  extension [S, A, B, Y] (m: Moore[Store[S, _] ~> Interface[A, B, _]])
    @scala.annotation.targetName("asPolyMapStoreToMono")
    def asPolyMap: PolyMap[Store[S, _], Interface[A, B, _], Y] =
      PolyMap(m.readout, m.update)   

  extension [F[_], S, A, B, Y] (m: Moore[StoreF[F, S, _] ~> Interface[A, F[B], _]])
    @scala.annotation.targetName("asPolyMapInterfaceTF[S], oMono")
    def asPolyMap: PolyMap[StoreF[F, S, _], Interface[A, F[B], _], Y] =
      PolyMap(m.readout, m.update)

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("asPolyMapStoreToBi")
    def asPolyMap: PolyMap[Store[S, _], BiInterface[A1, B1, A2, B2, _], Y] =
      PolyMap(m.readout, m.update)   

  extension [S1, S2, A1, B1, A2, B2, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToMonoMonoTensored")
    def asPolyMap: PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToMonoMonoMonoTensored")
    def asPolyMap: PolyMap[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]), (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToBiBiTensored")
    def asPolyMap: PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>  BiInterface[A1, A1 => B3, A2, A2 => B4, _]] )
    @scala.annotation.targetName("asPolyMapStoreTensoredToBiBiTensoredToBi")
    def asPolyMap: PolyMap[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]),  BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y] =
      PolyMap(m.readout, m.update)

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Store[S, _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _])])
    @scala.annotation.targetName("asPolyMapStoreToMonoMonoCartesian")
    def asPolyMap: PolyMap[Store[S, _], (Interface[A1, B1, _] × Interface[A2, B2, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S, A1, B1, A2, B2, A3, B3, Y] (m: Moore[Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])])
    @scala.annotation.targetName("asPolyMapStoreToMonoMonoCartesianTensoredMono")
    def asPolyMap: PolyMap[Store[S, _], ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S, A1, B1, A2, B2, A3, B3, Y] (m: Moore[Store[S, _] ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))])
    @scala.annotation.targetName("asPolyMapStoreToMonoMonoTensoredCartesianMono")
    def asPolyMap: PolyMap[Store[S, _], (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y] =
      PolyMap(m.readout, m.update)

  extension [S, A1, B1, A2, B2, A3, B3, Y] (m: Moore[(Store[Id[S], _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))])
    @scala.annotation.targetName("asPolyMapStoreToMonoMonoTensoredCartesianMonoCartesianTensoredMono")
    def asPolyMap: PolyMap[PolyMap[Store[Id[S], _], ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), _], (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y] =
      PolyMap(m.readout, m.update)