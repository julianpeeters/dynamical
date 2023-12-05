package dynamical.fsm

import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Store

type Init[P[_], Y] = P[Y] match
  case PolyMap[Store[s, _], p, Y]      => s
  case PolyMap[Store[s, _] ~> p, q, Y] => s
  case PolyMap[p, q, Y]                => Init[p, Y]

type Readout[P[_], Y] = P[Y] match
  case PolyMap[p, q, Y] => PolyMap.Phi[p, q, Y]
  
type Update[P[_], Y] = P[Y] match
  case PolyMap[p, q, Y] => PolyMap.PhiSharp[p, q, Y]