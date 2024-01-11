package dynamical.fsm

import cats.evidence.Is
import destructured.CtorOf
import dynamical.fsm.internal.{Init, Readout, Run, Update}
import dynamical.fsm.run.Runner2
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.⊗

trait Moore[P[_]]:
  def init[Y]: Init[P, Y]
  def readout[Y]: Readout[P, Y]
  def update[Y]: Update[P, Y]

object Moore:

  // Constructors /////////////////////////////////////////////////////////////
  def apply[S, A, B, Y](
    i: S,
    r: S => B,
    u: (S, A) => S
  ): Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]] =
    PolyMap[Monomial.Store[S, _], Monomial.Interface[A, B, _], Y](r, u)
      .asMoore(i)
  
  def apply[S, A1, B1, A2, B2, Y](
    i: S,
    r1: S => B1,
    r2: S => B2,
    u1: (S, A1) => S,
    u2: (S, A2) => S
  ): Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]] =
    PolyMap[Monomial.Store[S, _],  Binomial.Interface[A1, B1, A2, B2, _], Y]((r1, r2), (u1, u2))
      .asMoore(i)

  extension [S, A, B, Y] (p: PolyMap[Monomial.Store[S, _], Monomial.Interface[A, B, _], Y])
    @scala.annotation.targetName("asMooreStoreToMono")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]] =
      new Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _], Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[Monomial.Store[S, _], Binomial.Interface[A1, B1, A2, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBi")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]] =
      new Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _], Y] =
          p.`φ#`

  extension [S, A, B, Y] (p: PolyMap[PolyMap[Monomial.Store[S, _], Monomial.Interface[A, B, _], _], Monomial.Interface[A, A => B, _], Y])
    @scala.annotation.targetName("asMooreStoreToMonoToMono")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      new Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Monomial.Store[(S1, S2), _], Monomial.Interface[(A1, A2), (B1, B2), _], _ ], Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _], Y ])
    @scala.annotation.targetName("asMooreStoreToMonoToMonoTupled")
    def asMoore(i: (S1, S2)): Moore[Monomial.Store[(S1, S2), _] ~> Monomial.Interface[(A1, A2), (B1, B2), _] ~> Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]] =
      new Moore[Monomial.Store[(S1, S2), _] ~> Monomial.Interface[(A1, A2), (B1, B2), _] ~> Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]]:
        def init[Y]: Init[(Monomial.Store[(S1, S2), _]) ~> (Monomial.Interface[(A1, A2), (B1, B2), _]) ~> (Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[(S1, S2), _]) ~> (Monomial.Interface[(A1, A2), (B1, B2), _]) ~> (Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[(S1, S2), _]) ~> (Monomial.Interface[(A1, A2), (B1, B2), _]) ~> (Monomial.Interface[(A1, A2), (A1 => B1, A2 => B2), _]), Y] =
          p.`φ#`

  extension [S, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[Monomial.Store[S, _], Binomial.Interface[A1, B1, A2, B2, _], _], Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreToBiToBi")
    def asMoore(i: S): Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      new Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          i
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def update[Y]: Update[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], Monomial.Interface[A1, B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono1")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _]] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _]]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], Monomial.Interface[A1, B1, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono2")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _]] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _]]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, B1, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono3")
    def asMoore(i: (S1, S2)): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _], Y] =
          p.`φ#`

  extension [S1, S2, A, B, C, Y] (p: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, A => C, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMono4")
    def asMoore(i: (S1, S2)): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.`φ#`    

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], Monomial.Interface[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _]] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _], (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToMonoMonoTensorToTensoredMonoRunnable")
    def asMoore(i: (S1, S2)): Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])] =
      new Moore[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          i
        def readout[Y]: Readout[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.φ
        def update[Y]: Update[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), _] ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.`φ#`

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), _] , Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asMooreStoreStoreTensorToBiBiTensorToBi")
    def asMoore(i: (S1, S2)): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          i
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`

  // Lens Composition /////////////////////////////////////////////////////////
  extension [S, A, B, Y] (m: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]])
    @scala.annotation.targetName("andThenStoreMonoToMono")
    def andThen(
      w: Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]
    ): Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("andThenStoreBiToBi")
    def andThen(
      w: Wiring[Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]]
    ): Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, A, B, C, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono0")
    def andThen(
      w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono1")
    def andThen(
      w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
                                        

  extension [S1, S2, A1, B1, A2, B2, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMono2")
    def andThen(
      w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B2, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B2, _]] =
       m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoRunnable")
    def andThen(
      w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~>
      (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~>
        Monomial.Interface[A1, A1 => B2, _]
    ] =
      m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)
    @scala.annotation.targetName("andThenStoreStoreTensorMonoMonoTensorToMonoMonoTensor")
    def andThen(
      w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])]
    ): Moore[
      (Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~>
        (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~>
          (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])
    ] =
      m.asPolyMap
       .andThen(w.asPolyMap)
       .asMoore(m.init)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("andThenStoreStoreTensorBiBiTensorToBi")
    def andThen(
      w: Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]]
    )(using
      C: CtorOf[S1, A4],
      H: Is[B1, A3],
      I: Is[B2, A4],
      V: CtorOf[S2, A3]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      m.asPolyMap
       .andThen(w.asPolyMap, V.apply, C.apply, H.coerce, I.coerce)
       .asMoore(m.init)

  // Conversions //////////////////////////////////////////////////////////////
  extension [S, A, B, Y] (p: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]])
    @scala.annotation.targetName("asMealyStoreToMono")
    def asMealy: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]] =
      new Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[Monomial.Store[S, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, A => B, _]), Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S, A, B, Y] (p: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]])
    @scala.annotation.targetName("asMealyStoreToMonoToMono")
    def asMealy: Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      new Mealy[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]:
        def init[Y]: Init[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[(Monomial.Store[S, _] ~> Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S, _]) ~> (Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]), Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A, B, C, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono2")
    def asMealy: Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.readout
        def update[Y]: Update[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          (s: (S1, S2), a: A) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono")
    def asMealy: Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.readout
        def update[Y]: Update[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))
        
  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMonoMonoTensor")
    def asMealy: Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          p.readout
        def update[Y]: Update[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _]), Y] =
          (s, a) => (p.update(s, a), (p.readout(s)._1(a._1), p.readout(s)._2(a._2)))
        
  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] )
    def asMealy(using R: Runner2[((Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), (S1, S2), A1, B3, A2, B4]): Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Mealy[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.init
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.readout
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.update
        def run[Y]: Run[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> ((Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
         p.asPolyMap.run

  extension [S, A, B, Y] (m: Moore[Monomial.Store[S, _] ~> Monomial.Interface[A, B, _]])
    @scala.annotation.targetName("asPolyMapStoreToMono")
    def asPolyMap: PolyMap[Monomial.Store[S, _], Monomial.Interface[A, B, _], Y] =
      PolyMap(m.readout, m.update)   

  extension [S, A1, B1, A2, B2, Y] (m: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("asPolyMapStoreToBi")
    def asPolyMap: PolyMap[Monomial.Store[S, _], Binomial.Interface[A1, B1, A2, B2, _], Y] =
      PolyMap(m.readout, m.update)   

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToMonoMonoTensored")
    def asPolyMap: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])])
    @scala.annotation.targetName("asPolyMapStoreTensoredToBiBiTensored")
    def asPolyMap: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]), (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Y] =
      PolyMap(m.readout, m.update)

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m: Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~>  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] )
    @scala.annotation.targetName("asPolyMapStoreTensoredToBiBiTensoredToBi")
    def asPolyMap: PolyMap[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]),  Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
      PolyMap(m.readout, m.update)
  
  // Product //////////////////////////////////////////////////////////////////
  extension [S1, S2, A1, B1, A2, B2, Y] (m1: Moore[Monomial.Store[S1, _] ~> Monomial.Interface[A1, B1, _]])
    @scala.annotation.targetName("tensorStoreMonoXStoreMono")
    def ⊗(m2: Moore[Monomial.Store[S2, _] ~> Monomial.Interface[A2, B2, _]]): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])] =
      new Moore[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Y] =
          (s1, s2) => (m1.readout(s1), m2.readout(s2))
        def update[Y]: Update[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Y] =
          (s, a) => (m1.update(s._1, a._1), m2.update(s._2, a._2))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (m1: Moore[Monomial.Store[S1, _] ~> Binomial.Interface[A1, B1, A2, B2, _]])
    @scala.annotation.targetName("tensorStoreBiXStoreBi")
    def ⊗(
      m2: Moore[Monomial.Store[S2, _] ~> Binomial.Interface[A3, B3, A4, B4, _]]
    ): Moore[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])] =
      new Moore[(Monomial.Store[S1, _]) ⊗ (Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _])]:
        def init[Y]: Init[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Y] =
          (m1.init, m2.init)
        def readout[Y]: Readout[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Y] =
          (s => (m1.readout._1(s._1), m2.readout._1(s._2)), s => (m1.readout._2(s._1), m2.readout._2(s._2)))
        def update[Y]: Update[(Monomial.Store[S1, _] ⊗ Monomial.Store[S2, _]) ~> (Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Y] =
          ((s, a) =>  (m1.update._1(s._1, a._1), s._2), (s, a) => (s._1, m2.update._2(s._2, a._2)))

  // Swap //////////////////////////////////////////////////////////////////////////////
  extension [S, A1, B1, A2, B2, Y] (m: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B1, A2, B2, _]])
    def swapInterfaceDir: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A1, B2, A2, B1, _]] =
      m.asPolyMap.swapInterfaceDir.asMoore(m.init)
    def swapInterfacePos: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A2, B1, A1, B2, _]] =
      m.asPolyMap.swapInterfacePos.asMoore(m.init)
    def swapModes: Moore[Monomial.Store[S, _] ~> Binomial.Interface[A2, B2, A1, B1, _]] =
      m.asPolyMap.swapModes.asMoore(m.init)