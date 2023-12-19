package dynamical.fsm

import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial, Store}

object types:

  type Init[P[_], Y] = P[Y] match
    case PolyMap[Store[s, _], p, Y]             => s
    case PolyMap[Store[(s1, s2), _] ~> p, q, Y] => (s1, s2)
    case PolyMap[Store[s, _] ~> p, q, Y]        => s
    case PolyMap[p, q, Y]                       => Init[p, Y]
    case Store[(s1, s2), Y]                     => (s1, s2)

  type Readout[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => PolyMap.Phi[p, q, Y]

  type Run[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => (p[Y], q[Y]) match
      case (Store[s, Y],                Monomial[a, ? => b, Y]                     ) => (s, a) => (s, b)
      case (Store[s, Y],                Binomial[a1, ? => b1, a2, ? => b2, Y]      ) => (s, Unify2[a1, a2]) => (s, Unify2[b1, b2])
      case (Store[s, Y],                PolyMap[p, Monomial[a, ? => b, _], Y]      ) => (s, a) => (s, b)
      case ((Store[(s1, s2), Y]),       Monomial[(a1, a2), (? => b1, ? => b2), Y]  ) => ((s1, s2), (a1, a2)) => ((s1, s2), (b1, b2))
      case (PolyMap[Store[s, _], ?, Y], Monomial[a, ? => b, Y]                    ) => (s, a) => (s, b)
      case (PolyMap[Store[s, _], ?, Y], Binomial[a1, ? => b3, a2, ? => b4, Y]     ) => (s, Unify2[a1, a2]) => (s, Unify2[b3, b4])
      case (PolyMap[Store[s, _], ?, Y], Monomial[(a1, a2), (? => b1, ? => b2), Y] ) => (s, (a1, a2)) => (s, (b1, b2))

  type Update[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => PolyMap.PhiSharp[p, q, Y]

  type Unify2[U, V] = (U, V) match
    case (Some[a],     None.type  ) => Option[a]
    case (None.type,   Some[a]    ) => Option[a]
    case (Right[?, b], Left[a, ?] ) => Either[a, b]
    case (Left[a, ?],  Right[?, b]) => Either[a, b]