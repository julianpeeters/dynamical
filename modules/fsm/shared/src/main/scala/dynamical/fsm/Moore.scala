package dynamical.fsm

import dynamical.fsm.{Init, Readout, Update}
import polynomial.morphism.PolyMap
import polynomial.`object`.{Binomial, Monomial, Store}
import polynomial.morphism.~>
import polynomial.⊗

trait Moore[P[_]]:
  def init[Y]: Init[P, Y]
  def readout[Y]: Readout[P, Y]
  def update[Y]: Update[P, Y]

object Moore:

  def apply[S, A, B, Y](
    i: S,
    r: S => B,
    u: (S, A) => S
  ): Moore[Store[S, _] ~> Monomial[A, B, _]] =
    PolyMap[Store[S, _], Monomial[A, B, _], Y](r, u)
      .asMoore(i)
  
  def apply[S, A1, B1, A2, B2, Y](
    i: S,
    r1: S => B1,
    r2: S => B2,
    u1: (S, A1) => S,
    u2: (S, A2) => S
  ): Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]] =
    PolyMap[Store[S, _],  Binomial[A1, B1, A2, B2, _], Y]((r1, r2), (u1, u2))
      .asMoore(i)

  extension [S, A, B, Y] (m: Moore[Store[S, _] ~> Monomial[A, B, _]])
    @scala.annotation.targetName("andThen1")
    def andThen(
      w: Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]]
    ): Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("andThenBi1")
    def andThen(
      w: Wrapper[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]]
    ): Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)

  // extension [S1, S2, A, B, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Monomial[A, B, _] ⊗ Monomial[A, B, _])])
  //   @scala.annotation.targetName("andThen2")
  //   def andThen(
  //     w: Wrapper[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]]
  //   ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]] =
  //     m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)

  extension [S1, S2, A1, B1, A2, B2, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _])])
    @scala.annotation.targetName("andThen2")
    def andThen(
      w: Wrapper[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> Monomial[A1, A1 => B2, _]]
    ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> Monomial[A1, A1 => B2, _]] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("wrapTensored2")
    def andThen(
      w: Wrapper[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])]
    ): Moore[
      (Store[S1, _] ⊗ Store[S2, _]) ~>
        (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~>
          (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])
    ] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)


  extension [S, A, B, Y] (m: Moore[Store[S, _] ~> Monomial[A, B, _]])
    def andThen(
      r: B => A => B,
      u: (B, A) => A
    ): Moore[(Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _])] =
      PolyMap[PolyMap[Store[S, _], Monomial[A, B, _], _], Monomial[A, A => B, _], Y](
        m.readout.andThen(r),
        (s, a) => m.update(s, u(m.readout(s), a))
      ).asMoore(m.init)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("andThenOption")
    def andThen(
      w: Wrapper[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> Binomial[A1, A1 => B3,  A2, A2 => B4, _]]
    ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> Binomial[A1, A1 => B3,  A2, A2 => B4, _]] =
      ???
      // m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)      

  extension [S, A, B, Y] (p: PolyMap[Store[S, _], Monomial[A, B, _], Y])
    @scala.annotation.targetName("asMooreMono")
    def asMoore(i: S): Moore[Store[S, _] ~> Monomial[A, B, _]] =
      new Moore[Store[S, _] ~> Monomial[A, B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> Monomial[A, B, _], Y] =
          p.`φ#`

  extension [S, A, B, Y] (p: PolyMap[PolyMap[Store[S, _], Monomial[A, B, _], _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asMooreMonoMono")
    def asMoore(i: S): Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Store[S, _], Binomial[A1, B1, A2, B2, _], _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asMooreBiBi")
    def asMoore(i: S): Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
      new Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]]:
        def init[Y]: Init[Store[S, _] ~> Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), _], Monomial[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asMooreMonoMono2")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), _] ~> Monomial[A1, A1 => B2, _]] =
      new Moore[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), _] ~> Monomial[A1, A1 => B2, _]]:
        def init[Y]: Init[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), _] ~> Monomial[A1, A1 => B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), _] ~> Monomial[A1, A1 => B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), _] ~> Monomial[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Store[S, _], Binomial[A1, B1, A2, B2, _], Y])
    @scala.annotation.targetName("asMooreBi")
    def asMoore(i: S): Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]] =
      new Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]]:
        def init[Y]: Init[Store[S, _] ~> Binomial[A1, B1, A2, B2, _], Y] =
          i
        def readout[Y]: Readout[Store[S, _] ~> Binomial[A1, B1, A2, B2, _], Y] =
          p.φ
        def update[Y]: Update[Store[S, _] ~> Binomial[A1, B1, A2, B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (
    p: PolyMap[
      PolyMap[
        Store[(S1, S2), _],
        Monomial[(A1, A2), (B1, B2), _],
        _
      ],
      Monomial[(A1, A2), (A1 => B1, A2 => B2), _],
      Y
    ])
    @scala.annotation.targetName("asMooreAndThenTensored")
    def asMoore(i: (S1, S2)): Moore[Store[(S1, S2), _] ~> Monomial[(A1, A2), (B1, B2), _] ~> Monomial[(A1, A2), (A1 => B1, A2 => B2), _]] =
      new Moore[Store[(S1, S2), _] ~> Monomial[(A1, A2), (B1, B2), _] ~> Monomial[(A1, A2), (A1 => B1, A2 => B2), _]]:
        def init[Y]: Init[(Store[(S1, S2), _]) ~> (Monomial[(A1, A2), (B1, B2), _]) ~> (Monomial[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          i
        def readout[Y]: Readout[(Store[(S1, S2), _]) ~> (Monomial[(A1, A2), (B1, B2), _]) ~> (Monomial[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.φ
        def update[Y]: Update[(Store[(S1, S2), _]) ~> (Monomial[(A1, A2), (B1, B2), _]) ~> (Monomial[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.`φ#`

  extension [S, A, B, Y] (p: Moore[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]])
    @scala.annotation.targetName("asMealy3")
    def asMealy: Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[(Store[S, _] ~> Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          p.update
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, B, _] ~> Monomial[A, A => B, _]), Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  // extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m1: Moore[Store[S1, _] ~> Binomial[A1, B1, A2, B2, _]])
  //   @scala.annotation.targetName("tensor2")
  //   def ⊗(
  //     m2: Moore[Store[S2, _] ~> Binomial[A3, B3, A4, B4, _]]
  //   ): Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _])] =
  //     // ???
  //     // this is commented because maybe teh modes are conflicting?
  //     // maybe try alternating the modes instead of 
  //     new Moore[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _])]:
  //       def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Y] =
  //         (m1.init, m2.init)
  //       def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Y] =
  //         (s: (S1, S2)) => ()
  //         //(s: (S1, S2)) => (m1.readout._1(s._1), m2.readout._2(s._2))
  //         // ???
  //         // (s1: S1, s2: S2) => (m1.readout._1(s1), m2.readout._2(s2))// (a1, a2) => (m1.readout(s)(a1), m2.readout(t)(a2))
  //       def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Y] =
  //         ???

  extension [S, A, B, Y] (p: Moore[Store[S, _] ~> Monomial[A, A => B, _]])
    @scala.annotation.targetName("asMealyMono")
    def asMealy: Moore[Store[S, _] ~> Monomial[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Monomial[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Monomial[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Store[S, _] ~> Monomial[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[Store[S, _] ~> Monomial[A, A => B, _], Y] =
          p.update
        def run[Y]: Run[(Store[S, _]) ~> (Monomial[A, A => B, _]), Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, Y] (m1: Moore[Store[S1, _] ~> Monomial[A1, B1, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(m2: Moore[Store[S2, _] ~> Monomial[A2, B2, _]]): Moore[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _])] =
      new Moore[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _])]:
        def init[Y]: Init[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))// (a1, a2) => (m1.readout(s)(a1), m2.readout(t)(a2))
        def update[Y]: Update[(Store[S1, _]) ⊗ (Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))
  
  extension [S, A, B, Y] (m: Moore[Store[S, _] ~> Monomial[A, B, _]])
    @scala.annotation.targetName("asPolyMapMono")
    def asPolyMap: PolyMap[Store[S, _], Monomial[A, B, _], Y] =
      PolyMap(
        m.readout, 
        m.update
      )   

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Store[S, _] ~> Binomial[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("asPolyMapBi")
    def asPolyMap: PolyMap[Store[S, _], Binomial[A1, B1, A2, B2, _], Y] =
      PolyMap(
        m.readout, 
        m.update
      )   

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("asPolyMapTensoredBi")
    def asPolyMap: PolyMap[(Store[S1, _] ⊗ Store[S2, _]), (Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Y] =
      PolyMap(
        m.readout, 
        m.update
      )