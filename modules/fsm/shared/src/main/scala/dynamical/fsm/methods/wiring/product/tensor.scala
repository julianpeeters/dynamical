package dynamical.fsm.methods.wiring.product

import dynamical.fsm.methods.types.{Readout, Update}
import dynamical.fsm.Wiring
import polynomial.morphism.~>
import polynomial.`object`.Monomial.Interface
import polynomial.product.⊗

object tensor:
    
  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w1: Wiring[Interface[A1, B1, _] ~> Interface[A2, B2, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(w2: Wiring[Interface[A3, B3, _] ~> Interface[A4, B4, _]]): Wiring[(Interface[A1, B1, _]) ⊗ (Interface[A3, B3, _]) ~> (Interface[A2, B2, _] ⊗ Interface[A4, B4, _])] =
      new Wiring[(Interface[A1, B1, _]) ⊗ (Interface[A3, B3, _]) ~> (Interface[A2, B2, _] ⊗ Interface[A4, B4, _])]:
        def `f₁`[Y]: Readout[(Interface[A1, B1, _]) ⊗ (Interface[A3, B3, _]) ~> (Interface[A2, B2, _] ⊗ Interface[A4, B4, _]), Y] =
          (s1, s2) => (w1.`f₁`(s1), w2.`f₁`(s2))
        def `f#`[Y]: Update[(Interface[A1, B1, _]) ⊗ (Interface[A3, B3, _]) ~> (Interface[A2, B2, _] ⊗ Interface[A4, B4, _]), Y] =
          (s, a) => (w1.`f#`(s._1, a._1), w2.`f#`(s._2, a._2))