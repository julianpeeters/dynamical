package dynamical.fsm.methods.mealy.run

import dynamical.fsm.methods.types.{Unify2}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.product.⊗

trait Runner2[P[_], S, A1, B1, A2, B2]:
  extension [Y] (p: P[Y])
    def run: (S, Unify2[A1, A2]) => (S, Unify2[B1, B2])

object Runner2:

  given optionRunner2a[S, A, B]: Runner2[
    PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => Some[B], None.type, None.type => None.type, _], _],
    S, Some[A], Some[B], None.type, None.type
  ] =
    new Runner2[
      PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => Some[B], None.type, None.type => None.type, _], _],
      S, Some[A], Some[B], None.type, None.type
    ]:
      extension [Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => Some[B], None.type, None.type => None.type, _], Y])
        def run: (S, Unify2[Some[A], None.type]) => (S, Unify2[Some[B], None.type]) =
          (s, a) =>
            a match
              case v@Some(value) => (p.`φ#`._1(s, v), p.φ._1(s)(v))
              case None => (p.`φ#`._2(s, None), p.φ._2(s)(None))

  given optionRunner2b[S, A, B]: Runner2[
    PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => None.type, Some[A], Some[A] => Some[B], _], _],
    S, None.type, None.type, Some[A], Some[B]] =
    new Runner2[
      PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => None.type, Some[A], Some[A] => Some[B], _], _],
      S, None.type, None.type, Some[A], Some[B]]:
      extension [Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => None.type,Some[A], Some[A] => Some[B], _], Y])
        def run: (S, Unify2[None.type, Some[A]]) => (S, Unify2[None.type, Some[B]]) =
          (s, a) =>
            a match
              case v@Some(value) => (p.`φ#`._2(s, v), p.φ._2(s)(v))
              case None => (p.`φ#`._1(s, None), p.φ._1(s)(None))

  given optionRunner2c[S, A, B]: Runner2[
    PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], _],
    S, Some[A], None.type, None.type, Some[B]
  ] =
    new Runner2[
      PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], _],
      S, Some[A], None.type, None.type, Some[B]
    ]:
      extension [Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y])
        def run: (S, Unify2[Some[A], None.type]) => (S, Unify2[None.type, Some[B]]) =
          (s, a) =>
            a match
              case v@Some(value) => (p.`φ#`._1(s, v), p.φ._1(s)(v))
              case None => (p.`φ#`._2(s, None), p.φ._2(s)(None))

  given optionRunner2d[S, A, B]: Runner2[
    PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => Some[B], Some[A], Some[A] => None.type, _], _],
    S, None.type, Some[B], Some[A], None.type
  ] =
    new Runner2[
      PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => Some[B], Some[A], Some[A] => None.type, _], _],
      S, None.type, Some[B], Some[A], None.type
    ]:
      extension [Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => Some[B], Some[A], Some[A] => None.type, _], Y])
        def run: (S, Unify2[None.type, Some[A]]) => (S, Unify2[Some[B], None.type]) =
          (s, a) =>
            a match
              case v@Some(value) => (p.`φ#`._2(s, v), p.φ._2(s)(v))
              case None => (p.`φ#`._1(s, None), p.φ._1(s)(None))

  given optionRunner2e[S, A, B]: Runner2[
    PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => Some[B], None.type, None.type => None.type, _], _],
    S, Some[A], Some[B], None.type, None.type
  ] =
    new Runner2[
      PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => Some[B], None.type, None.type => None.type, _], _],
      S, Some[A], Some[B], None.type, None.type
    ]:
      extension [Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => Some[B], None.type, None.type => None.type, _], Y])
        def run: (S, Unify2[Some[A], None.type]) => (S, Unify2[Some[B], None.type]) =
          (s, a) =>
            a match
              case v@Some(value) => (p.`φ#`._1(s, v), p.φ._1(s)(v))
              case None => (p.`φ#`._2(s, None), p.φ._2(s)(None))

  given optionRunner2f[S, A, B]: Runner2[
    PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => None.type, Some[A], Some[A] => Some[B], _], _],
    S, None.type, None.type, Some[A], Some[B]] =
    new Runner2[
      PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => None.type, Some[A], Some[A] => Some[B], _], _],
      S, None.type, None.type, Some[A], Some[B]]:
      extension [Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => None.type,Some[A], Some[A] => Some[B], _], Y])
        def run: (S, Unify2[None.type, Some[A]]) => (S, Unify2[None.type, Some[B]]) =
          (s, a) =>
            a match
              case v@Some(value) => (p.`φ#`._2(s, v), p.φ._2(s)(v))
              case None => (p.`φ#`._1(s, None), p.φ._1(s)(None))

  // given optionRunner2g[S, A, B]: Runner2[
  //   PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], _],
  //   S, Some[A], None.type, None.type, Some[B]
  // ] =
  //   new Runner2[
  //     PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], _],
  //     S, Some[A], None.type, None.type, Some[B]
  //   ]:
  //     def run[Y](
  //       p: PolyMap[Monomial.Store[S, _], Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y]
  //     ): (S, Unify2[Some[A], None.type]) => (S, Unify2[None.type, Some[B]]) =
  //       (s, a) =>
  //         a match
  //           case v@Some(value) => (p.`φ#`._1(s, v), p.φ._1(s)(v))
  //           case None => (p.`φ#`._2(s, None), p.φ._2(s)(None))

  given optionRunner2h[S, A, B]: Runner2[
    PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => Some[B], Some[A], Some[A] => None.type, _], _],
    S, None.type, Some[B], Some[A], None.type
  ] =
    new Runner2[
      PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => Some[B], Some[A], Some[A] => None.type, _], _],
      S, None.type, Some[B], Some[A], None.type
    ]:
      extension [Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[None.type, None.type => Some[B], Some[A], Some[A] => None.type, _], Y])
        def run: (S, Unify2[None.type, Some[A]]) => (S, Unify2[Some[B], None.type]) =
          (s, a) =>
            a match
              case v@Some(value) => (p.`φ#`._2(s, v), p.φ._2(s)(v))
              case None => (p.`φ#`._1(s, None), p.φ._1(s)(None))

  given optionRunner2x[S1, S2, A, B]: Runner2[
    (Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~>
      (Binomial.Interface[Some[A], None.type, None.type, Some[String], _] ⊗ Binomial.Interface[None.type, None.type, Some[String], Some[B], _]) ~>
        (Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]),
    (S1, S2),
    Some[A],
    None.type,
    None.type,
    Some[B]
  ] =
    new Runner2[
      (Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~>
        (Binomial.Interface[Some[A], None.type, None.type, Some[String], _] ⊗ Binomial.Interface[None.type, None.type, Some[String], Some[B], _]) ~>
          (Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]),
      (S1, S2),
      Some[A],
      None.type,
      None.type,
      Some[B]
    ]:
      extension [Y] (p: ((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~>
        (Binomial.Interface[Some[A], None.type, None.type, Some[String], _] ⊗ Binomial.Interface[None.type, None.type, Some[String], Some[B], _]) ~>
          (Binomial.Interface[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]))[Y]
      )
        def run: ((S1, S2), Unify2[Some[A], None.type]) => ((S1, S2), Unify2[None.type, Some[B]]) =
          (s, a) => a match
            case v@Some(value) => (p.`φ#`._1(s, v), p.φ._1(s)(v))
            case None => (p.`φ#`._2(s, None), p.φ._2(s)(None))
        