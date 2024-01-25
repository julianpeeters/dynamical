package dynamical.fsm.methods

import polynomial.morphism.PolyMap
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.{Composition, Tensor}

object types:

  type Codomain1[X] = X match
    case Function1[a, b] => b
    case (Function1[a1, b1], Function1[a2, b2]) => (b1, b2)

  type Codomain2[X] = X match
    case (b1, Function[a2, b2]) => (b1, b2)

  type Init[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y]            => Init[p, Y]
    case Monomial.Store[s, Y]        => s
    case Monomial.Interface[a, b, Y] => b
    case Tensor[p, q, Y]             => (Init[p, Y], Init[q, Y]) 

  type Readout[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => PolyMap.Phi[p, q, Y]

  type Run[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => (p[Y], q[Y]) match
      case (Monomial.Store[s, Y], Binomial.Interface[a1, b1, a2, b2, Y]) => (s, Unify2[a1, a2]) => (s, Unify2[Codomain1[b1], Codomain1[b2]])
      case (Monomial.Store[s, Y], Monomial.Interface[a, b, Y])           => (s, a) => (s, Codomain1[b])
      case (Monomial.Store[s, Y], Composition[p, q, Y])                  => (s, Composition.DecomposeSharp[p, q, Y]) => (s, Codomain2[Composition.Decompose[p, q, Y]])
      case (Monomial.Store[s, Y], PolyMap[p, q, Y])                      => Run[PolyMap[Monomial.Store[s, _], q, _], Y]
      case (PolyMap[p, q, Y],     Binomial.Interface[a1, b3, a2, b4, Y]) => Run[PolyMap[p, Binomial.Interface[a1, b3, a2, b4, _], _], Y]
      case (PolyMap[p, q, Y],     Monomial.Interface[a, b, Y])           => Run[PolyMap[p, Monomial.Interface[a, b, _], _], Y]
      case (PolyMap[o, p, Y],     Tensor[q, r, Y])                       => Run[PolyMap[o, Tensor.DayConvolution[q, r, _], _], Y]
      case (Tensor[p, q, Y],      Binomial.Interface[a1, b3, a2, b4, Y]) => ((Init[p, Y], Init[q, Y]), Unify2[a1, a2]) => ((Init[p, Y], Init[q, Y]), Unify2[Codomain1[b3], Codomain1[b4]])
      case (Tensor[p, q, Y],      Monomial.Interface[a, b, Y])           => ((Init[p, Y], Init[q, Y]), a) => ((Init[p, Y], Init[q, Y]), Codomain1[b])
      case (Tensor[o, p, Y],      Tensor[q, r, Y])                       => Run[PolyMap[Tensor.DayConvolution[o, p, _], Tensor.DayConvolution[q, r, _], _], Y]

  type Update[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => PolyMap.PhiSharp[p, q, Y]

  type Unify2[U, V] = (U, V) match
    case (Some[a],     None.type  ) => Option[a]
    case (None.type,   Some[a]    ) => Option[a]
    case (Right[?, b], Left[a, ?] ) => Either[a, b]
    case (Left[a, ?],  Right[?, b]) => Either[a, b]