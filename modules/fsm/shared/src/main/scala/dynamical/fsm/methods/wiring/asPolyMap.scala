package dynamical.fsm.methods.wiring

import cats.Id
import dynamical.fsm.Wiring
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Bi, Mono}
import polynomial.product.⊗

object asPolyMap:

  extension [A, B, Y] (w: Wiring[Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Id[B], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMono")
    def asPolyMap: PolyMap[Mono.Interface[A, Id[B], _], Mono.Interface[A, A => Id[B], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, Y] (w: Wiring[Mono.Interface[A, Id[B], _] ~> Mono.Interface[A, A => Option[B], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMonoPrism")
    def asPolyMap: PolyMap[Mono.Interface[A, Id[B], _], Mono.Interface[A, A => Option[B], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Bi.Interface[A1, B1, A2, B2, _]) ~> (Bi.Interface[A1, A1 => B1, A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapWiringBiBi")
    def asPolyMap: PolyMap[Bi.Interface[A1, B1, A2, B2, _], Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, B2, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono1")
    def asPolyMap: PolyMap[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Mono.Interface[A1, B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)
                            
  extension [A1, B1, A2, B2, Y] (w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, B1, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono2")
    def asPolyMap: PolyMap[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Mono.Interface[A1, B1, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono3")
    def asPolyMap: PolyMap[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]), Mono.Interface[A, C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono4")
    def asPolyMap: PolyMap[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]), Mono.Interface[A, A => C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asPolyMap1")
    def asPolyMap: PolyMap[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Mono.Interface[A1, A1 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> (Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapTensored")
    def asPolyMap: PolyMap[Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _], Mono.Interface[A1, A1 => B1, _] ⊗ Mono.Interface[A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, I, O, Y] (w: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~>  Mono.Interface[I, I => O, _]])
    @scala.annotation.targetName("asPolyMapTensored2")
    def asPolyMap: PolyMap[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]), Mono.Interface[I, I => O, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w: Wiring[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> (Bi.Interface[A1, A1 => B3, A2, A2 => B4, _])])
    @scala.annotation.targetName("asPolyMapTensoredBi")
    def asPolyMap: PolyMap[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]), (Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
      PolyMap(w.`f₁`, w.`f#`)
