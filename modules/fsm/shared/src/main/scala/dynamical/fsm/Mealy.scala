package dynamical.fsm

import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Monomial, Binomial, Store}
import dynamical.fsm.{Init, Readout, Update}
import polynomial.⊗

trait Mealy[P[_]] extends Moore[P]:
  def run[Y]: Run[P, Y]

object Mealy:

  def apply[S, A, B, Y](
    i: S,
    r: S => A => B,
    u: (S, A) => S
  ): Mealy[Store[S, _] ~> Monomial[A, A => B, _]] =
    PolyMap[Store[S, _], Monomial[A, A => B, _], Y](r, u).asMealy(i)

  def apply[S, A, B, Y](
    i: S,
    r1: S => Some[A] => None.type,
    r2: S => None.type => Some[B],
    u1: (S, Some[A]) => S,
    u2: (S, None.type) => S
  ): Mealy[Store[S, _] ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]] =
    PolyMap[Store[S, _],  Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y]((r1, r2), (u1, u2))
      .asMealy(i)
  
  extension [S1, S2, A1, B1, A2, B2, Y] (m1: Mealy[Store[S1, _] ~> Monomial[A1, A1 => B1, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(m2: Mealy[Store[S2, _] ~> Monomial[A2, A2 => B2, _]]): Mealy[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])] =
      new Mealy[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])]:
        def init[Y]: Init[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))// (a1, a2) => (m1.readout(s)(a1), m2.readout(t)(a2))
        def update[Y]: Update[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))
        def run[Y]: Run[(Store[S1, _]) ⊗ (Store[S2, _]) ~> ((Monomial[A1, A1 => B1, _]) ⊗ (Monomial[A2, A2 => B2, _])), Y] =
          (s, a) => (update(s, a), (readout(s)._1(a._1), readout(s)._2(a._2)))

  // extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m1: Mealy[Store[S1, _] ~> Binomial[A1, B1, A2, B2, _]])
  //   @scala.annotation.targetName("tensor2")
  //   def ⊗(
  //     m2: Mealy[Store[S2, _] ~> Binomial[A3, B3, A4, B4, _]]
  //   ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _])] =
  //     // ???
  //     // this is commented because maybe teh modes are conflicting?
  //     // maybe try alternating the modes instead of 
  //     new Moore[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _])]:
  //       def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Y] =
  //         (m1.init, m2.init)
  //       def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Y] =
  //         ???
  //         // ???
  //         // (s1: S1, s2: S2) => (m1.readout._1(s1), m2.readout._2(s2))// (a1, a2) => (m1.readout(s)(a1), m2.readout(t)(a2))
  //       def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Y] =
  //         ???
  //         // (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))
  //       // def run[Y]: Run[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, A1 => B1, A2, A2 => B2, _] ⊗ Binomial[A3, A3 => B3, A4, A4 => B4, _]), Y] =
  //       //   ???
  //       //   (s, a) => (update(s, a), (readout(s)._1(a._1), readout(s)._2(a._2)))
      
  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asMealyPolyMono")
    def asMealy(i: S): Mealy[Store[S, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[(Store[S, _]) ~> Monomial[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          p.φ
        def update[Y]: Update[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          (s: S, a: A) => (p.`φ#`(s, a), p.φ(s)(a))

  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y])
    @scala.annotation.targetName("asMealyOptionOptionBi")
    def asMealy(i: S): Mealy[Store[S, _] ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]] =
      new Mealy[Store[S, _] ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]]:
        def init[Y]: Init[(Store[S, _]) ~> Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _], Y] =
          i
        def readout[Y]: Readout[(Store[S, _]) ~> (Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]), Y] =
          p.φ
        def update[Y]: Update[(Store[S, _]) ~> (Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Binomial[Some[A], Some[A] => None.type, None.type, None.type => Some[B], _]), Y] =
          (s, a) =>
            a match
              case Some(value) => (p.`φ#`._1(s, Some(value)), p.φ._1(s)(Some(value))) 
              case None => (p.`φ#`._2(s, None), p.φ._2(s)(None)) 

  extension [S, A, B, Y] (p: (Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _])[Y])
    @scala.annotation.targetName("asMealy2")
    def asMealy(i: S): Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[(Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          p.`φ#`
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          (s, a) => (p.`φ#`(s, a), p.φ(s)(a))