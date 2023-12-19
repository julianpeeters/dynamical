package dynamical.fsm

import dynamical.fsm.types.{Readout, Update}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.⊗

trait Wiring[P[_]]:
  def `f₁`[Y]: Readout[P, Y]
  def `f#`[Y]: Update[P, Y]

object Wiring:

  @scala.annotation.targetName("appMono")
  def apply[A, B, Y](
    r: B => A => B,
    u: (B, A) => A
  ): Wiring[Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
    PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y](r, u).asWiring

  @scala.annotation.targetName("appBi")
  def apply[A1, B1, A2, B2, Y](
    r1: B1 => A1 => B1,
    r2: B2 => A2 => B2,
    u1: (B1, A1) => A1 => B1,
    u2: (B2, A2) => A2 => B2
  ): Wiring[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
    PolyMap[Binomial[A1, B1, A2, B2, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y]((r1, r2), (u1, u2)).asWiring

  @scala.annotation.targetName("appTensored1")
  def apply[A, B, Y](
    r: ((B, B)) => A => B,
    u: ((B, B), A) => (A, A)
  ): Wiring[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]] =
    PolyMap[(Monomial[A, B, _] ⊗ Monomial[A, B, _]), Monomial[A, A => B, _], Y](r, u).asWiring

  @scala.annotation.targetName("appTensored2")
  def apply[A, B, C, Y](
    r: ((C, B)) => A => C,
    u: ((C, B), A) => ((A, B), C)
  ): Wiring[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]) ~> Monomial[A, A => C, _]] =
    PolyMap[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]), Monomial[A, A => C, _], Y](r, u).asWiring  

  @scala.annotation.targetName("appBiTensored1")
  def apply[A1, B1, A2, B2, A3, B3, A4, B4, Y](
    r1: ((B1, B3)) => A1 => B3,
    r2: ((B2, B4)) => A2 => B4,
    u1: ((B1, B3), A1) => A1 => B3,
    u2: ((B2, B4), A2) => A2 => B4
  ): Wiring[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> Binomial[A1, A1 => B3, A2, A2 => B4, _]] =
    PolyMap[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Binomial[A1, A1 => B3, A2, A2 => B4, _], Y]((r1, r2), (u1, u2)).asWiring

  def serially[A1, B1, A2, B2, A3, B3, A4, B4, Y]: Wiring[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[B1, B1, B2, B3, _]) ~> Binomial[A1, A1 => B1, A2, A2 => B3, _]] =
    PolyMap[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[B1, B1, B2, B3, _]),  Binomial[A1, A1 => B1, A2, A2 => B3, _], Y](
      (
        (_, b1) => _ => b1,
        (_, b3) => a2 => b3
      ),(
        (b, a1) => (_ => b._2),
        (b, a2) => (_ => b._2)
      )
    ).asWiring

  extension [A, B, Y] (w: Wiring[Monomial[A, B, _] ~> Monomial[A, A => B, _]])
    def asPolyMap: PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Binomial[A1, B1, A2, B2, _]) ~> (Binomial[A1, A1 => B1, A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapWiringBiBi")
    def asPolyMap: PolyMap[Binomial[A1, B1, A2, B2, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> Monomial[A1, B2, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono1")
    def asPolyMap: PolyMap[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), Monomial[A1, B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> Monomial[A1, B1, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono2")
    def asPolyMap: PolyMap[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), Monomial[A1, B1, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  // extension [A, B, C, Y] (w: Wiring[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]) ~> Monomial[A, C, _]])
  //   @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono3")
  //   def asPolyMap: PolyMap[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]), Monomial[A, C, _], Y] =
  //     PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]) ~> Monomial[A, A => C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono4")
    def asPolyMap: PolyMap[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]), Monomial[A, A => C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> Monomial[A1, A1 => B2, _]])
    @scala.annotation.targetName("asPolyMap1")
    def asPolyMap: PolyMap[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), Monomial[A1, A1 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapTensored")
    def asPolyMap: PolyMap[Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _], Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w: Wiring[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> (Binomial[A1, A1 => B3, A2, A2 => B4, _])])
    @scala.annotation.targetName("asPolyMapTensoredBi")
    def asPolyMap: PolyMap[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), (Binomial[A1, A1 => B3, A2, A2 => B4, _]), Y] =
      PolyMap(w.`f₁`, w.`f#`)

  def aspm[A1, B1, A2, B2, A3, B3, A4, B4, Y](w: Wiring[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> (Binomial[A1, A1 => B3, A2, A2 => B4, _])]): PolyMap[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), (Binomial[A1, A1 => B3, A2, A2 => B4, _]), Y] =
    PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, Y] (p: PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asWiringMonoMono")
    def asWiring: Wiring[Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Wiring[Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def `f₁`[Y]: Readout[Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[Binomial[A1, B1, A2, B2, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asWiringBiBi")
    def asWiring: Wiring[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
      new Wiring[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]]:
        def `f₁`[Y]: Readout[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [A, B, Y] (p: PolyMap[(Monomial[A, B, _] ⊗ Monomial[A, B, _]), Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored1")
    def asWiring: Wiring[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]] =
      new Wiring[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]]:
        def `f₁`[Y]: Readout[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _], Y] =
          p.`φ#`

  extension [A, B, C, Y] (p: PolyMap[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]), Monomial[A, A => C, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored2")
    def asWiring: Wiring[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]) ~> Monomial[A, A => C, _]] =
      new Wiring[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]) ~> Monomial[A, A => C, _]]:
        def `f₁`[Y]: Readout[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]) ~> Monomial[A, A => C, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Monomial[(A, B), C, _] ⊗ Monomial[C, B, _]) ~> Monomial[A, A => C, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]), Binomial[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asWiringBiBiTensored")
    def asWiring: Wiring[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> Binomial[A1, A1 => B3, A2, A2 => B4, _]] =
      new Wiring[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> Binomial[A1, A1 => B3, A2, A2 => B4, _]]:
        def `f₁`[Y]: Readout[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> Binomial[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Binomial[A1, B1, A2, B2, _] ⊗ Binomial[A3, B3, A4, B4, _]) ~> Binomial[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`
          
  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w1: Wiring[Monomial[A1, B1, _] ~> Monomial[A2, B2, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(w2: Wiring[Monomial[A3, B3, _] ~> Monomial[A4, B4, _]]): Wiring[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _])] =
      new Wiring[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _])]:
        def `f₁`[Y]: Readout[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _]), Y] =
          (s1, s2) => (w1.`f₁`(s1), w2.`f₁`(s2))
        def `f#`[Y]: Update[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _]), Y] =
          (s, a) => (w1.`f#`(s._1, a._1), w2.`f#`(s._2, a._2))