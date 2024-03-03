package dynamical.fsm.methods.mealy.product

import dynamical.fsm.Mealy
import dynamical.fsm.methods.types.{Init, Readout, Run, Update}
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.product.⊗

object tensor:

  extension [S1, S2, A1, B1, A2, B2] (m1: Mealy[Store[S1, _] ~> Interface[A1, A1 => B1, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(m2: Mealy[Store[S2, _] ~> Interface[A2, A2 => B2, _]]): Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])] =
      new Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))
        def run[Y]: Run[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          (s, a) => (update(s, a), (readout(s)._1(a._1), readout(s)._2(a._2)))