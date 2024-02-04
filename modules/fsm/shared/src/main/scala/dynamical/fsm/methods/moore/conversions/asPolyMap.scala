package dynamical.fsm.methods.moore.conversions

import dynamical.fsm.Moore
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Bi, Mono}
import polynomial.product.⊗

object asPolyMap:

  extension [S, A, B, Y] (m: Moore[Mono.Store[S, _] ~> Mono.Interface[A, B, _]])
    @scala.annotation.targetName("asPolyMapStoreToMono")
    def asPolyMap: PolyMap[Mono.Store[S, _], Mono.Interface[A, B, _], Y] =
      PolyMap(m.readout, m.update)   

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("asPolyMapStoreToBi")
    def asPolyMap: PolyMap[Mono.Store[S, _], Bi.Interface[A1, B1, A2, B2, _], Y] =
      PolyMap(m.readout, m.update)   

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToMonoMonoTensored")
    def asPolyMap: PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]) ~> (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToMonoMonoMonoTensored")
    def asPolyMap: PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _] ⊗ Mono.Store[S3, _]), (Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToBiBiTensored")
    def asPolyMap: PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]), (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~>  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]] )
    @scala.annotation.targetName("asPolyMapStoreTensoredToBiBiTensoredToBi")
    def asPolyMap: PolyMap[(Mono.Store[S1, _] ⊗ Mono.Store[S2, _]) ~> (Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]),  Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
      PolyMap(m.readout, m.update)
