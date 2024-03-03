package dynamical.fsm.methods.polymap

import dynamical.fsm.methods.types.{Readout, Update}
import dynamical.fsm.Wiring
import polynomial.morphism.{PolyMap, ~>}
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.Interface
import polynomial.product.{×, ⊗}

object asWiring:

  extension [A, B, Y] (p: PolyMap[Interface[A, B, _], Interface[A, A => B, _], Y])
    @scala.annotation.targetName("asWiringMonoMono")
    def asWiring: Wiring[Interface[A, B, _] ~> Interface[A, A => B, _]] =
      new Wiring[Interface[A, B, _] ~> Interface[A, A => B, _]]:
        def `f₁`[Y]: Readout[Interface[A, B, _] ~> Interface[A, A => B, _], Y] =
          p.φ
        def `f#`[Y]: Update[Interface[A, B, _] ~> Interface[A, A => B, _], Y] =
          p.`φ#`

  extension [F[_], A, B, C, D, Y] (p: PolyMap[Interface[A, B, _], Interface[C, C => F[D], _], Y])
    @scala.annotation.targetName("asWiringMonoMonoF")
    def asWiring: Wiring[Interface[A, B, _] ~> Interface[C, C => F[D], _]] =
      new Wiring[Interface[A, B, _] ~> Interface[C, C => F[D], _]]:
        def `f₁`[Y]: Readout[Interface[A, B, _] ~> Interface[C, C => F[D], _], Y] =
          p.φ
        def `f#`[Y]: Update[Interface[A, B, _] ~> Interface[C, C => F[D], _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[BiInterface[A1, B1, A2, B2, _], BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y])
    @scala.annotation.targetName("asWiringBiBi")
    def asWiring: Wiring[BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]] =
      new Wiring[BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]]:
        def `f₁`[Y]: Readout[BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[BiInterface[A1, B1, A2, B2, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[(Interface[A1, B1, _] × Interface[A2, B2, _]), Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y])
    // @scala.annotation.targetName("asWiringMonoMonoCartesian")
    def asWiring: Wiring[(Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]] =
      // ???
      new Wiring[(Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]]:
        def `f₁`[Y]: Readout[(Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          p.φ
        def `f#`[Y]: Update[(Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          p.`φ#`

  // extension [A, B, Y] (p: PolyMap[(Interface[A, B, _] ⊗ Interface[A, B, _]), Interface[A, A => B, _], Y])
  //   @scala.annotation.targetName("asWiringMonoMonoTensored1")
  //   def asWiring: Wiring[(Interface[A, B, _] ⊗ Interface[A, B, _]) ~> Interface[A, A => B, _]] =
  //     new Wiring[(Interface[A, B, _] ⊗ Interface[A, B, _]) ~> Interface[A, A => B, _]]:
  //       def `f₁`[Y]: Readout[(Interface[A, B, _] ⊗ Interface[A, B, _]) ~> Interface[A, A => B, _], Y] =
  //         p.φ
  //       def `f#`[Y]: Update[(Interface[A, B, _] ⊗ Interface[A, B, _]) ~> Interface[A, A => B, _], Y] =
  //         p.`φ#`

  extension [A1, B1, A2, B2, Y] (p: PolyMap[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]), Interface[A1, A1 => B2, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored2")
    def asWiring: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]] =
      new Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]]:
        def `f₁`[Y]: Readout[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, I, O, Y] (p: PolyMap[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]), Interface[I, I => O, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored3")
    def asWiring: Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _]] =
      new Wiring[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _]]:
        def `f₁`[Y]: Readout[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          p.`φ#`


  extension [A, B, C, Y] (p: PolyMap[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]), Interface[A, A => C, _], Y])
    @scala.annotation.targetName("asWiringMonoMonoTensored4")
    def asWiring: Wiring[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]] =
      new Wiring[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]]:
        def `f₁`[Y]: Readout[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          p.φ
        def `f#`[Y]: Update[(Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          p.`φ#`

  extension [A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: PolyMap[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]), BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y])
    @scala.annotation.targetName("asWiringBiBiTensored")
    def asWiring: Wiring[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Wiring[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _]]:
        def `f₁`[Y]: Readout[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.φ
        def `f#`[Y]: Update[(BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~> BiInterface[A1, A1 => B3, A2, A2 => B4, _], Y] =
          p.`φ#`
      