package dynamical.fsm

// import dynamical.fsm.Moore.asMoore
import dynamical.fsm.{Readout, Update}
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.⊗

trait Wrapper[P[_]]:
  def `f₁`[Y]: Readout[P, Y]
  def `f#`[Y]: Update[P, Y]

object Wrapper:

  @scala.annotation.targetName("appMono")
  def apply[A, B, Y](
    r: B => A => B,
    u: (B, A) => A
  ): Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
    PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y](r, u).asWrapper

  @scala.annotation.targetName("appBi")
  def apply[A1, B1, A2, B2, Y](
    r1: B1 => A1 => B1,
    r2: B2 => A2 => B2,
    u1: B1 => A1 => B1,
    u2: B2 => A2 => B2
  ): Wrapper[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
    PolyMap[Binomial[A1, B1, A2, B2, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y]((r1, r2), (u1, u2)).asWrapper

  @scala.annotation.targetName("appTensored1")
  def apply[A, B, Y](
    r: ((B, B)) => A => B,
    u: ((B, B), A) => (A, A)
  ): Wrapper[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]] =
    PolyMap[(Monomial[A, B, _] ⊗ Monomial[A, B, _]), Monomial[A, A => B, _], Y](r, u).asWrapper

  extension [A, B, Y] (w: Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]])
    def asPolyMap: PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wrapper[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]])
    @scala.annotation.targetName("asPolyMapWrapperBiBi")
    def asPolyMap: PolyMap[Binomial[A1, B1, A2, B2, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [A1, B1, A2, B2, Y] (w: Wrapper[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> Monomial[A1, A1 => B2, _]])
    @scala.annotation.targetName("asPolyMap1")
    def asPolyMap: PolyMap[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]), Monomial[A1, A1 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)

  extension [S1, S2, A1, B1, A2, B2, Y] (w: Wrapper[(Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~> (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])])
    @scala.annotation.targetName("asPolyMapTensored")
    def asPolyMap: PolyMap[Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _], Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _], Y] =
      PolyMap(w.`f₁`, w.`f#`)
    // @scala.annotation.targetName("wrapTensored2")
    // def wrap(
    //   m: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _])]
    // ): Moore[
    //   (Store[S1, _] ⊗ Store[S2, _]) ~>
    //     (Monomial[A1, B1, _] ⊗ Monomial[A2, B2, _]) ~>
    //       (Monomial[A1, A1 => B1, _] ⊗ Monomial[A2, A2 => B2, _])
    // ] =
    //   m.asPolyMap.andThen(w.asPolyMap).asMoore(m.init)

  extension [A, B, Y] (p: PolyMap[Monomial[A, B, _], Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asWrapperMonoMono")
    def asWrapper: Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]] =
      new Wrapper[Monomial[A, B, _] ~> Monomial[A, A => B, _]]:
        def `f₁`[Y]: Readout[Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[Monomial[A, B, _] ~> Monomial[A, A => B, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[Binomial[A1, B1, A2, B2, _], Binomial[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asWrapperBiBi")
    def asWrapper: Wrapper[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]] =
      new Wrapper[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _]]:
        def `f₁`[Y]: Readout[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[Binomial[A1, B1, A2, B2, _] ~> Binomial[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [A, B, Y] (p: PolyMap[(Monomial[A, B, _] ⊗ Monomial[A, B, _]), Monomial[A, A => B, _], Y])
    @scala.annotation.targetName("asWrapperMonoMonoTensored")
    def asWrapper: Wrapper[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]] =
      new Wrapper[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _]]:
        def `f₁`[Y]: Readout[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Monomial[A, B, _] ⊗ Monomial[A, B, _]) ~> Monomial[A, A => B, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (w1: Wrapper[Monomial[A1, B1, _] ~> Monomial[A2, B2, _]])
    @scala.annotation.targetName("tensor1")
    def ⊗(w2: Wrapper[Monomial[A3, B3, _] ~> Monomial[A4, B4, _]]): Wrapper[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _])] =
      new Wrapper[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _])]:
        def `f₁`[Y]: Readout[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _]), Y] =
          (s1, s2) => (w1.`f₁`(s1), w2.`f₁`(s2))
        def `f#`[Y]: Update[(Monomial[A1, B1, _]) ⊗ (Monomial[A3, B3, _]) ~> (Monomial[A2, B2, _] ⊗ Monomial[A4, B4, _]), Y] =
          (s, a) => (w1.`f#`(s._1, a._1), w2.`f#`(s._2, a._2))