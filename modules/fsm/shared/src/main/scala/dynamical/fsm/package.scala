package dynamical.fsm

import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Monomial, Binomial, Store}

type Init[P[_], Y] = P[Y] match
  case Store[(s1, s2), Y]               => (s1, s2)
  case PolyMap[Store[s, _], p, Y]       => s
  case PolyMap[Store[s, _] ~> p, q, Y]  => s
  case PolyMap[p, q, Y]                 => Init[p, Y]

type Readout[P[_], Y] = P[Y] match
  case PolyMap[p, q, Y] => PolyMap.Phi[p, q, Y]

type Update[P[_], Y] = P[Y] match
  case PolyMap[p, q, Y] => PolyMap.PhiSharp[p, q, Y]

type Run[P[_], Y] = P[Y] match
  case PolyMap[p, q, Y] => (p[Y], q[Y]) match
    case (Store[s, Y], Monomial[a, ? => b, Y])                                                         => (s, a) => (s, b)
    case (Store[s, Y], Binomial[Some[?], Some[a1] => None.type, None.type, None.type => Some[b2], Y])  => (s, Option[a1]) => (s, Option[b2])
    case (Store[s, Y], PolyMap[p, Monomial[a, ? => b, _], Y])                                          => (s, a) => (s, b)
    case (PolyMap[Store[s, _], ?, Y], Monomial[a, ? => b, Y])                                          => (s, a) => (s, b)
    case ((Store[(s1, s2), Y]), (Monomial[(a1, a2), (? => b1, ? => b2), Y]))  =>  ((s1, s2), (a1, a2)) => ((s1, s2), (b1, b2))