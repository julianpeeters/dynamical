package dynamical.fsm

import cats.Id
import dynamical.fsm.methods.types.{Readout, Update}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.⊗

trait Wiring[P[_]]:
  def `f₁`[Y]: Readout[P, Y]
  def `f#`[Y]: Update[P, Y]

object Wiring:

  // @scala.annotation.targetName("appMono")
  // def apply[A, B, Y](
  //   r: B => A => B,
  //   u: (B, A) => A
  // ): Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
  //   PolyMap[Monomial.Interface[A, B, _], Monomial.Interface[A, A => B, _], Y](r, u).asWiring

  @scala.annotation.targetName("appMonF")
  def apply[F[_], A, B, Y](
    r: B => A => F[B],
    u: (B, A) => A
  ): Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => F[B], _]] =
    PolyMap[Monomial.Interface[A, B, _], Monomial.Interface[A, A => F[B], _], Y](r, u).asWiring


  @scala.annotation.targetName("appBi")
  def apply[A1, B1, A2, B2, Y](
    r1: B1 => A1 => B1,
    r2: B2 => A2 => B2,
    u1: (B1, A1) => A1,
    u2: (B2, A2) => A2
  ): Wiring[Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
    PolyMap[Binomial.Interface[A1, B1, A2, B2, _], Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y]((r1, r2), (u1, u2)).asWiring

  @scala.annotation.targetName("appTensored1")
  def apply[A1, B1, A2, B2, Y](
    r: ((B1, B2)) => A1 => B2,
    u: ((B1, B2), A1) => (A1, A2)
  ): Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]] =
    PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, A1 => B2, _], Y](r, u).asWiring

  @scala.annotation.targetName("appTensored2")
  def apply[A1, B1, A2, B2, A3, B3, I, O, Y](
    r: (((B1, B2), B3)) => I => O,
    u: (((B1, B2), B3), I) => ((A1, A2), A3)
  ): Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _]] =
    PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]), Monomial.Interface[I, I => O, _], Y](r, u).asWiring

  @scala.annotation.targetName("appTensored3")
  def apply[A, B, C, Y](
    r: ((C, B)) => A => C,
    u: ((C, B), A) => ((A, B), C)
  ): Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
    PolyMap[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, A => C, _], Y](r, u).asWiring  

  @scala.annotation.targetName("appBiTensored1")
  def apply[A1, B1, A2, B2, A3, B3, A4, B4, Y](
    r1: ((B1, B3)) => A1 => B3,
    r2: ((B2, B4)) => A2 => B4,
    u1: ((B1, B3), A1) => (A1, A3),
    u2: ((B2, B4), A2) => (A2, A4)
  ): Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
    PolyMap[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y]((r1, r2), (u1, u2)).asWiring

  @scala.annotation.targetName("appTensored4")
  def serially[A1, B1, A2, B2, B3, Y]: Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[B1, B1, B2, B3, _]) ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B3, _]] =
    PolyMap[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[B1, B1, B2, B3, _]),  Binomial.Interface[A1, A1 => B1, A2, A2 => B3, _], Y](
      (
        (_, b1) => a1 => b1,
        (_, b3) => a2 => b3
      ),(
        (b, a1) => (a1, b._2), 
        (b, a2) => (a2, b._1)
      )
    ).asWiring

  extension [A, B, Y] (w: Wiring[Monomial.Interface[A, Id[B], _] ~> Monomial.Interface[A, A => Id[B], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMono")
    def asPolyMap: PolyMap[Monomial.Interface[A, Id[B], _], Monomial.Interface[A, A => Id[B], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, Y] (w: Wiring[Monomial.Interface[A, Id[B], _] ~> Monomial.Interface[A, A => Option[B], _]])
    @scala.annotation.targetName("asPolyMapWiringMonoMonoPrism")
    def asPolyMap: PolyMap[Monomial.Interface[A, Id[B], _], Monomial.Interface[A, A => Option[B], _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Binomial.Interface[A1, B1, A2, B2, _]) ~> (Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapWiringBiBi")
    def asPolyMap: PolyMap[Binomial.Interface[A1, B1, A2, B2, _], Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B2, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono1")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)
                            
  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, B1, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono2")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, B1, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono3")
    def asPolyMap: PolyMap[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, C, Y] (w: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]])
    @scala.annotation.targetName("asPolyMapMonoMonoTensoredToMono4")
    def asPolyMap: PolyMap[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, A => C, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asPolyMap1")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, A1 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> (Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapTensored")
    def asPolyMap: PolyMap[Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _], Monomial.Interface[A1, A1 => B1, _] ⊗ Monomial.Interface[A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, I, O, Y] (w: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~>  Monomial.Interface[I, I => O, _]])
    @scala.annotation.targetName("asPolyMapTensored2")
    def asPolyMap: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]), Monomial.Interface[I, I => O, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w: Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _])])
    @scala.annotation.targetName("asPolyMapTensoredBi")
    def asPolyMap: PolyMap[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), (Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A, B, Y] (p: PolyMap[Monomial.Interface[A, B, _], Monomial.Interface[A, A => B, _], Y])
    @scala.annotation.targetName("asWiringMonoMono")
    def asWiring: Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]] =
      new Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _]]:
        def `f₁`[Y]: Readout[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => B, _], Y] =
          p.`φ#`

  extension [F[_], A, B, Y] (p: PolyMap[Monomial.Interface[A, B, _], Monomial.Interface[A, A => F[B], _], Y])
    @scala.annotation.targetName("asWiringMonoMonoF")
    def asWiring: Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => F[B], _]] =
      new Wiring[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => F[B], _]]:
        def `f₁`[Y]: Readout[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => F[B], _], Y] =
          p.φ
        def `f#`[Y]: Update[Monomial.Interface[A, B, _] ~> Monomial.Interface[A, A => F[B], _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[Binomial.Interface[A1, B1, A2, B2, _], Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asWiringBiBi")
    def asWiring: Wiring[Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      new Wiring[Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _]]:
        def `f₁`[Y]: Readout[Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[Binomial.Interface[A1, B1, A2, B2, _] ~> Binomial.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  // extension [A, B, Y] (p: PolyMap[(Monomial.Interface[A, B, _] ⊗ Monomial.Interface[A, B, _]), Monomial.Interface[A, A => B, _], Y])
  //   @scala.annotation.targetName("asWiringMonoMonoTensored1")
  //   def asWiring: Wiring[(Monomial.Interface[A, B, _] ⊗ Monomial.Interface[A, B, _]) ~> Monomial.Interface[A, A => B, _]] =
  //     new Wiring[(Monomial.Interface[A, B, _] ⊗ Monomial.Interface[A, B, _]) ~> Monomial.Interface[A, A => B, _]]:
  //       def `f₁`[Y]: Readout[(Monomial.Interface[A, B, _] ⊗ Monomial.Interface[A, B, _]) ~> Monomial.Interface[A, A => B, _], Y] =
  //         p.φ
  //       def `f#`[Y]: Update[(Monomial.Interface[A, B, _] ⊗ Monomial.Interface[A, B, _]) ~> Monomial.Interface[A, A => B, _], Y] =
  //         p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]), Monomial.Interface[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored2")
    def asWiring: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]] =
      new Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _]]:
        def `f₁`[Y]: Readout[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _]) ~> Monomial.Interface[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, I, O, Y] (p: PolyMap[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]), Monomial.Interface[I, I => O, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored3")
    def asWiring: Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _]] =
      new Wiring[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _]]:
        def `f₁`[Y]: Readout[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Monomial.Interface[A1, B1, _] ⊗ Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A3, B3, _]) ~> Monomial.Interface[I, I => O, _], Y] =
          p.`φ#`


  extension [A, B, C, Y] (p: PolyMap[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]), Monomial.Interface[A, A => C, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored4")
    def asWiring: Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]] =
      new Wiring[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _]]:
        def `f₁`[Y]: Readout[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Monomial.Interface[(A, B), C, _] ⊗ Monomial.Interface[C, B, _]) ~> Monomial.Interface[A, A => C, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]), Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asWiringBiBiTensored")
    def asWiring: Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Wiring[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def `f₁`[Y]: Readout[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Binomial.Interface[A1, B1, A2, B2, _] ⊗ Binomial.Interface[A3, B3, A4, B4, _]) ~> Binomial.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`
          
  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w1: Wiring[Monomial.Interface[A1, B1, _] ~> Monomial.Interface[A2, B2, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(w2: Wiring[Monomial.Interface[A3, B3, _] ~> Monomial.Interface[A4, B4, _]]): Wiring[(Monomial.Interface[A1, B1, _]) ⊗ (Monomial.Interface[A3, B3, _]) ~> (Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A4, B4, _])] =
      new Wiring[(Monomial.Interface[A1, B1, _]) ⊗ (Monomial.Interface[A3, B3, _]) ~> (Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A4, B4, _])]:
        def `f₁`[Y]: Readout[(Monomial.Interface[A1, B1, _]) ⊗ (Monomial.Interface[A3, B3, _]) ~> (Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A4, B4, _]), Y] =
          (s1, s2) => (w1.`f₁`(s1), w2.`f₁`(s2))
        def `f#`[Y]: Update[(Monomial.Interface[A1, B1, _]) ⊗ (Monomial.Interface[A3, B3, _]) ~> (Monomial.Interface[A2, B2, _] ⊗ Monomial.Interface[A4, B4, _]), Y] =
          (s, a) => (w1.`f#`(s._1, a._1), w2.`f#`(s._2, a._2))