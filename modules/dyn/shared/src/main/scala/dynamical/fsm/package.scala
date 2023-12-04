package dynamical.fsm

import polynomial.functor.Pos
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Monomial, Store}

type Init[P[_], Y] = P[Y] match
  case PolyMap[Store[s, _], p, Y]      => s
  case PolyMap[Store[s, _] ~> p, q, Y] => s
  case PolyMap[p, q, Y]                => Init[p, Y]

type Readout[P[_], Y] = P[Y] match
  case PolyMap[Monomial[a1, b1, _], Monomial[a2, b2, _], Y] => b1 => b2
  case PolyMap[Store[s, _], p, Y]                           => s => Pos[p, Y]
  case PolyMap[Store[s, _], Monomial[a, b, _], Y]           => s => b
  case PolyMap[Store[s, _] ~> p, Monomial[a, b, _], Y]      => s => b
  case PolyMap[Store[s, _] ~> p, q, Y]                      => s => Readout[q, Y]
  
type Update[P[_], Y] = P[Y] match
  case PolyMap[Monomial[a1, b1, _], Monomial[a2, b2, _], Y] => (b1, a2) => a1
  case PolyMap[Store[s, _], Monomial[a, b, _], Y]           => (s, a) => s
  case PolyMap[Store[s, _] ~> p, Monomial[a, b, _], Y]      => (s, a) => s
  case PolyMap[p, q, Y]                                     => Update[p, Y]