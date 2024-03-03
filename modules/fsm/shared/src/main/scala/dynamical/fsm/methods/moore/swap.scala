package dynamical.fsm.methods.moore

import dynamical.fsm.methods.polymap.asMoore.asMoore
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.Store

object swap:

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Store[S, _] ~> BiInterface[A1, B1, A2, B2, _]])

    def swapInterfaceDir: Moore[Store[S, _] ~> BiInterface[A1, B2, A2, B1, _]] =
      m.asPolyMap.swapInterfaceDir.asMoore(m.init)

    def swapInterfacePos: Moore[Store[S, _] ~> BiInterface[A2, B1, A1, B2, _]] =
      m.asPolyMap.swapInterfacePos.asMoore(m.init)

    def swapModes: Moore[Store[S, _] ~> BiInterface[A2, B2, A1, B1, _]] =
      m.asPolyMap.swapModes.asMoore(m.init)