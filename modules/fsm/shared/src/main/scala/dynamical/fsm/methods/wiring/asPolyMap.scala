package dynamical.fsm.methods.wiring

import cats.Id
import dynamical.fsm.Wiring
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.Interface
import polynomial.product.{×, ⊗}

object asPolyMap:

  extension [A, B, C, D, Y] (w: Wiring[Interface[A, Id[B], _] ~> Interface[C, C => Id[D], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMono")
    def asPolyMap: PolyMap[Interface[A, Id[B], _], Interface[C, C => Id[D], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, Y] (w: Wiring[Interface[A, Id[B], _] ~> Interface[A, A => Option[B], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMonoPrism")
    def asPolyMap: PolyMap[Interface[A, Id[B], _], Interface[A, A => Option[B], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(BiInterface[A1, B1, A2, B2, _]) ~> (BiInterface[A1, A1 => B1, A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapWiringBiBi")
    def asPolyMap: PolyMap[BiInterface[A1, B1, A2, B2, _], BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, B2, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono1")
    def asPolyMap: PolyMap[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), Interface[A1, B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)
                            
  extension [A1, B1, A2, B2, Y] (w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, B1, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono2")
    def asPolyMap: PolyMap[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), Interface[A1, B1, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono3")
    def asPolyMap: PolyMap[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]), Interface[A, C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono4")
    def asPolyMap: PolyMap[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]), Interface[A, A => C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asPolyMap1")
    def asPolyMap: PolyMap[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), Interface[A1, A1 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapTensored")
    def asPolyMap: PolyMap[Interface[A1, B1, _] ⊗ Interface[A2, B2, _], Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, I, O, Y] (w: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~>  Interface[I, I => O, _]])
    @scala.annotation.targetName("asPolyMapTensored2")
    def asPolyMap: PolyMap[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]), Interface[I, I => O, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w: Wiring[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> (BiInterface[A1, A1 => B3, A2, A2 => B4, _])])
    @scala.annotation.targetName("asPolyMapTensoredBi")
    def asPolyMap: PolyMap[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]), (BiInterface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]])
    @scala.annotation.targetName("asPolyMapMonoMonoCartesianToMono1")
    def asPolyMap: PolyMap[(Interface[A1, B1, _] × Interface[A2, B2, _]), Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, Y] (w: Wiring[((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]) ~> Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]])
    @scala.annotation.targetName("asPolyMapMonoMonoCartesianToMonoTensoredMono")
    def asPolyMap: PolyMap[((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), Interface[(Either[A1, A2], A3), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, Y] (w: Wiring[(Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])) ~> Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _]])
    @scala.annotation.targetName("asPolyMapToCartesianMonoTensoredMono")
    def asPolyMap: PolyMap[(Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Interface[Either[A1, (A2, A3)], Either[A1, (A2, A3)] => (B1, (B2, B3)), _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, Y] (w: Wiring[((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]) ~> (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _]))])
    @scala.annotation.targetName("asPolyMapMonoMonoCartesianToMonoTensoredMonoTensoredCartesian")
    def asPolyMap: PolyMap[((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _]), (Interface[A1, B1, _] × (Interface[A2, B2, _] ⊗ Interface[A3, B3, _])), Y] =
      PolyMap(w.`f₁`, w.`f#`)