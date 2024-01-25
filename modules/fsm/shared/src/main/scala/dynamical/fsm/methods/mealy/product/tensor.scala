package dynamical.fsm.methods.mealy.product

import dynamical.fsm.methods.types.{Init, Readout, Run, Update}
import dynamical.fsm.Mealy
import polynomial.morphism.~>
import polynomial.`object`.Monomial
import polynomial.product.⊗

object tensor:

  extension [S1, S2, A1, B1, A2, B2, Y] (m1: Mealy[Monomial.Store[S1, _] ~> Monomial.Interface[A1, A1 => B1, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(m2: Mealy[Monomial.Store[S2, _] ~> Monomial.Interface[A2, A2 => B2, _]]): Mealy[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])] =
      new Mealy[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))
        def run[Y]: Run[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> ((Monomial.Interface[A1, A1 => B1, _]) ⊗ (Monomial.Interface[A2, A2 => B2, _])), Y] =
          (s, a) => (update(s, a), (readout(s)._1(a._1), readout(s)._2(a._2)))