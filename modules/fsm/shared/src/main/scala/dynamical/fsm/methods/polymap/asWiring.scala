package dynamical.fsm.methods.polymap

import dynamical.fsm.methods.types.{Readout, Update}
import dynamical.fsm.Wiring
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Binomial, Monomial}
import polynomial.product.⊗

object asWiring:

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
      