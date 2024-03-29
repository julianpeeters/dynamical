package dynamical.fsm.methods

import cats.effect.IO
import polynomial.morphism.PolyMap
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store, StoreF}
import polynomial.product.{Composition, Tensor}

object types:

  type Codomain1[X] = X match
    case Function1[a, b]                        => b
    case (Function1[a1, b1], Function1[a2, b2]) => (b1, b2)

  type Codomain2[X] = X match
    case (b1, Function[a2, b2]) => (b1, b2)

  type Init[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y]   => Init[p, Y]
    case Store[s, Y]        => s
    case StoreF[f, s, Y]    => s
    case Interface[a, b, Y] => b
    case Tensor[p, q, Y]    => (Init[p, Y], Init[q, Y]) 

  type Readout[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => PolyMap.Phi[p, q, Y]

  type Run[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => (PolyMap.Eval[p, Y], PolyMap.Eval[q, Y]) match
      case (Store[s, Y], BiInterface[a1, b1, a2, b2, Y])      => (s, Unify2[a1, a2]) => (s, Unify2[Codomain1[b1], Codomain1[b2]])
      case (StoreF[f, s, Y], Interface[a, b, Y])              => (s, a) => Runout[f[s], b]
      case (Store[s, Y], Interface[a, b, Y])                  => (s, a) => (s, Codomain1[b])
      case (Store[s, Y], Composition[p, q, Y])                => (s, Composition.DecomposeSharp[p, q, Y]) => (s, Codomain2[Composition.Decompose[p, q, Y]])
      case (Store[s, Y], PolyMap[p, q, Y])                    => Run[PolyMap[Store[s, _], q, _], Y]
      case (PolyMap[p, q, Y], BiInterface[a1, b3, a2, b4, Y]) => Run[PolyMap[p, BiInterface[a1, b3, a2, b4, _], _], Y]
      case (PolyMap[p, q, Y], Interface[a, b, Y])             => Run[PolyMap[p, Interface[a, b, _], _], Y]
      case (PolyMap[o, p, Y], Tensor[q, r, Y])                => Run[PolyMap[o, Tensor.DayConvolution[q, r, _], _], Y]

  type Runout[S, B] = (S, B) match
    case (IO[s], a => IO[b]) => IO[(s, b)]

  type Update[P[_], Y] = P[Y] match
    case PolyMap[p, q, Y] => PolyMap.PhiSharp[p, q, Y]

  type Unify2[U, V] = (U, V) match
    case (Some[a],     None.type  ) => Option[a]
    case (None.type,   Some[a]    ) => Option[a]
    case (Right[?, b], Left[a, ?] ) => Either[a, b]
    case (Left[a, ?],  Right[?, b]) => Either[a, b]