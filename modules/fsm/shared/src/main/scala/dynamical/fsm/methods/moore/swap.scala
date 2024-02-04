package dynamical.fsm.methods.moore

import dynamical.fsm.methods.polymap.asMoore.asMoore
import dynamical.fsm.Moore
import polynomial.morphism.~>
import polynomial.`object`.{Bi, Mono}

object swap:

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B1, A2, B2, _]])

    def swapInterfaceDir: Moore[Mono.Store[S, _] ~> Bi.Interface[A1, B2, A2, B1, _]] =
      m.asPolyMap.swapInterfaceDir.asMoore(m.init)

    def swapInterfacePos: Moore[Mono.Store[S, _] ~> Bi.Interface[A2, B1, A1, B2, _]] =
      m.asPolyMap.swapInterfacePos.asMoore(m.init)

    def swapModes: Moore[Mono.Store[S, _] ~> Bi.Interface[A2, B2, A1, B1, _]] =
      m.asPolyMap.swapModes.asMoore(m.init)