package dynamical.fsm.methods.wiring

import cats.Id
import dynamical.fsm.Wiring
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.⊗

object asPolyMap:

  extension [A, B, Y] (w: Wiring[Monomial.Interface[A, Id[B], _] ~> Monomial.Interface[A, A => Id[B], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMono")
    def asPolyMap: PolyMap[Monomial.Interface[A, Id[B], _], Monomial.Interface[A, A => Id[B], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, Y] (w: Wiring[Monomial.Interface[A, Id[B], _] ~> Monomial.Interface[A, A => Option[B], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMonoPrism")
    def asPolyMap: PolyMap[Monomial.Interface[A, Id[B], _], Monomial.Interface[A, A => Option[B], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Binomial.Interface[A1, B1, A2, B2, _]) ~> (Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapWiringBiBi")
    def asPolyMap: PolyMap[Binomial.Interface[A1, B1, A2, B2, _], Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B2, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono1")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)
                            
  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B1, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono2")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, B1, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono3")
    def asPolyMap: PolyMap[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono4")
    def asPolyMap: PolyMap[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, A => C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asPolyMap1")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, A1 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapTensored")
    def asPolyMap: PolyMap[Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _], Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, I, O, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~>  Monomial.Interface[I, I => O, _]])
    @scala.annotation.targetName("asPolyMapTensored2")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]), Monomial.Interface[I, I => O, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w: Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _])])
    @scala.annotation.targetName("asPolyMapTensoredBi")
    def asPolyMap: PolyMap[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
      PolyMap(w.`f₁`, w.`f#`)
