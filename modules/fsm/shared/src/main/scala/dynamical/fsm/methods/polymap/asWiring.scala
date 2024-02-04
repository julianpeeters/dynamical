package dynamical.fsm.methods.polymap

import dynamical.fsm.methods.types.{Readout, Update}
import dynamical.fsm.Wiring
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.{Bi, Mono}
import polynomial.product.⊗

object asWiring:

  extension [A, B, Y] (p: PolyMap[Mono.Interface[A, B, _], Mono.Interface[A, A => B, _], Y])
    @scala.annotation.targetName("asWiringMonoMono")
    def asWiring: Wiring[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _]] =
      new Wiring[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _]]:
        def `f₁`[Y]: Readout[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => B, _], Y] =
          p.`φ#`

  extension [F[_], A, B, Y] (p: PolyMap[Mono.Interface[A, B, _], Mono.Interface[A, A => F[B], _], Y])
    @scala.annotation.targetName("asWiringMonoMonoF")
    def asWiring: Wiring[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => F[B], _]] =
      new Wiring[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => F[B], _]]:
        def `f₁`[Y]: Readout[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => F[B], _], Y] =
          p.φ
        def `f#`[Y]: Update[Mono.Interface[A, B, _] ~> Mono.Interface[A, A => F[B], _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[Bi.Interface[A1, B1, A2, B2, _], Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asWiringBiBi")
    def asWiring: Wiring[Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _]] =
      new Wiring[Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _]]:
        def `f₁`[Y]: Readout[Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[Bi.Interface[A1, B1, A2, B2, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  // extension [A, B, Y] (p: PolyMap[(Mono.Interface[A, B, _] ⊗ Mono.Interface[A, B, _]), Mono.Interface[A, A => B, _], Y])
  //   @scala.annotation.targetName("asWiringMonoMonoTensored1")
  //   def asWiring: Wiring[(Mono.Interface[A, B, _] ⊗ Mono.Interface[A, B, _]) ~> Mono.Interface[A, A => B, _]] =
  //     new Wiring[(Mono.Interface[A, B, _] ⊗ Mono.Interface[A, B, _]) ~> Mono.Interface[A, A => B, _]]:
  //       def `f₁`[Y]: Readout[(Mono.Interface[A, B, _] ⊗ Mono.Interface[A, B, _]) ~> Mono.Interface[A, A => B, _], Y] =
  //         p.φ
  //       def `f#`[Y]: Update[(Mono.Interface[A, B, _] ⊗ Mono.Interface[A, B, _]) ~> Mono.Interface[A, A => B, _], Y] =
  //         p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]), Mono.Interface[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored2")
    def asWiring: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]] =
      new Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _]]:
        def `f₁`[Y]: Readout[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _]) ~> Mono.Interface[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, I, O, Y] (p: PolyMap[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]), Mono.Interface[I, I => O, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored3")
    def asWiring: Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _]] =
      new Wiring[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _]]:
        def `f₁`[Y]: Readout[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Mono.Interface[A1, B1, _] ⊗ Mono.Interface[A2, B2, _] ⊗ Mono.Interface[A3, B3, _]) ~> Mono.Interface[I, I => O, _], Y] =
          p.`φ#`


  extension [A, B, C, Y] (p: PolyMap[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]), Mono.Interface[A, A => C, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored4")
    def asWiring: Wiring[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]] =
      new Wiring[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _]]:
        def `f₁`[Y]: Readout[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Mono.Interface[(A, B), C, _] ⊗ Mono.Interface[C, B, _]) ~> Mono.Interface[A, A => C, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]), Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asWiringBiBiTensored")
    def asWiring: Wiring[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Wiring[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _]]:
        def `f₁`[Y]: Readout[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Bi.Interface[A1, B1, A2, B2, _] ⊗ Bi.Interface[A3, B3, A4, B4, _]) ~> Bi.Interface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`
      