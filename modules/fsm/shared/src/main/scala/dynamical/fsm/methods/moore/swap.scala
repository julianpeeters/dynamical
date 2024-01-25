package dynamical.fsm.methods.moore

import dynamical.fsm.methods.polymap.asMoore.asMoore
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.{Binomial, Monomial}

object swap:

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]])

    def swapInterfaceDir: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B2, A2, B1, _]] =
      m.asPolyMap.swapInterfaceDir.asMoore(m.init)

    def swapInterfacePos: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A2, B1, A1, B2, _]] =
      m.asPolyMap.swapInterfacePos.asMoore(m.init)

    def swapModes: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A2, B2, A1, B1, _]] =
      m.asPolyMap.swapModes.asMoore(m.init)