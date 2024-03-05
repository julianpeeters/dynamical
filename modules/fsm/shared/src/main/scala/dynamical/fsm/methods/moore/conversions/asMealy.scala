package dynamical.fsm.methods.moore.conversions

import cats.Id
import dynamical.fsm.methods.mealy.run.Runner2
import dynamical.fsm.methods.types.{Init, Readout, Run, Update}
import dynamical.fsm.{Mealy, Moore}
import polynomial.morphism.~>
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store}
import polynomial.product.{×, ⊗, ◁}

object asMealy:
    
  extension [S, A, B, Y] (p: Moore[Store[S, _] ~> Interface[A, A => B, _]])
    @scala.annotation.targetName("asMealyStoreToMono")
    def asMealy: Moore[Store[S, _] ~> Interface[A, A => B, _]] =
      new Mealy[Store[S, _] ~> Interface[A, A => B, _]]:
        def init[Y]: Init[Store[S, _] ~> Interface[A, A => B, _], Y] =
          p.init
        def readout[Y]: Readout[Store[S, _] ~> Interface[A, A => B, _], Y] =
          p.readout
        def update[Y]: Update[Store[S, _] ~> Interface[A, A => B, _], Y] =
          p.update
        def run[Y]: Run[(Store[S, _]) ~> (Interface[A, A => B, _]), Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S, A, B, C, D, Y] (p: Moore[Store[S, _] ~> Interface[A, B, _] ~> Interface[C, C => D, _]])
    @scala.annotation.targetName("asMealyStoreToMonoToMono")
    def asMealy: Mealy[Store[S, _] ~> Interface[A, B, _] ~> Interface[C, C => D, _]] =
      new Mealy[Store[S, _] ~> Interface[A, B, _] ~> Interface[C, C => D, _]]:
        def init[Y]: Init[Store[S, _] ~> Interface[A, B, _] ~> Interface[C, C => D, _], Y] =
          p.init
        def readout[Y]: Readout[Store[S, _] ~> Interface[A, B, _] ~> Interface[C, C => D, _], Y] =
          p.readout
        def update[Y]: Update[(Store[S, _] ~> Interface[A, B, _] ~> Interface[C, C => D, _]), Y] =
          p.update
        def run[Y]: Run[(Store[S, _]) ~> (Interface[A, B, _] ~> Interface[C, C => D, _]), Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  extension [S, A1, B1, A2, B2, Y] (p: Moore[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]])
    @scala.annotation.targetName("asMealyStoreToCartesianMono")
    def asMealy: Mealy[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]] =
      new Mealy[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _]]:
        def init[Y]: Init[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          p.init
        def readout[Y]: Readout[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          p.readout
        def update[Y]: Update[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          p.update
        def run[Y]: Run[Store[Id[S], _] ~> (Interface[A1, B1, _] × Interface[A2, B2, _]) ~> Interface[Either[A1, A2], Either[A1, A2] => (B1, B2), _], Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: Moore[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[((Either[A1, A2], A3)), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]])
    @scala.annotation.targetName("asMealyStoreToCartesianMonoTensored")
    def asMealy: Mealy[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[((Either[A1, A2], A3)), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]] =
      new Mealy[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[((Either[A1, A2], A3)), ((Either[A1, A2], A3)) => ((B1, B2), B3), _]]:
        def init[Y]: Init[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[((Either[A1, A2], A3)), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y] =
          p.init
        def readout[Y]: Readout[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[((Either[A1, A2], A3)), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y] =
          p.readout
        def update[Y]: Update[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[((Either[A1, A2], A3)), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y] =
          p.update
        def run[Y]: Run[(Store[S, _] ~> ((Interface[A1, B1, _] × Interface[A2, B2, _]) ⊗ Interface[A3, B3, _])) ~> Interface[((Either[A1, A2], A3)), ((Either[A1, A2], A3)) => ((B1, B2), B3), _], Y] =
          (s, a) => (update(s, a), readout(s)(a._1, a._2))

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: Moore[(Store[S, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _]])
    @scala.annotation.targetName("asMealyStoreToTensoredMonoCartesian")
    def asMealy: Mealy[(Store[S, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _]] =
      new Mealy[(Store[S, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _]]:
        def init[Y]: Init[(Store[S, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          p.init
        def readout[Y]: Readout[(Store[S, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          p.readout
        def update[Y]: Update[(Store[S, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          p.update
        def run[Y]: Run[(Store[S, _] ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S, A1, B1, A2, B2, A3, B3, Y] (p: Moore[((Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])) ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _] ])
    @scala.annotation.targetName("asMealyStoreToTensoredMonoCartesianToCartesianMonoTensored")
    def asMealy: Mealy[((Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])) ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _]] =
      new Mealy[((Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])) ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _]]:
        def init[Y]: Init[((Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])) ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          p.init
        def readout[Y]: Readout[((Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])) ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          p.readout
        def update[Y]: Update[((Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])) ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          p.update
        def run[Y]: Run[((Store[String, _] ~> ((Interface[Int, Int, _] × Interface[String, String, _]) ⊗ Interface[Boolean, Boolean, _])) ~> (Interface[Int, Int, _] × (Interface[String, String, _] ⊗ Interface[Boolean, Boolean, _]))) ~> Interface[Either[Int, (String, Boolean)], Either[Int, (String, Boolean)] => (Int, (String, Boolean)), _], Y] =
          (s, a) => (update(s, a), readout(s)(a))

  extension [S1, S2, A, B, C, Y] (p: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono2")
    def asMealy: Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]] =
      new Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          p.init
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          p.readout
        def update[Y]: Update[((Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _]), Y] =
          p.update
        def run[Y]: Run[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[(A, B), C, _] ⊗ Interface[C, B, _]) ~> Interface[A, A => C, _], Y] =
          (s: (S1, S2), a: A) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMono")
    def asMealy: Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]] =
      new Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _], Y] =
          p.init
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _], Y] =
          p.readout
        def update[Y]: Update[((Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _]), Y] =
          p.update
        def run[Y]: Run[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> Interface[A1, A1 => B2, _], Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))
        
  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asMealyStoreStoreTensorToMonoMonoTensorToMonoMonoTensor")
    def asMealy: Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])] =
      new Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          p.init
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          p.readout
        def update[Y]: Update[((Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _])), Y] =
          p.update
        def run[Y]: Run[(Store[S1, _] ⊗ Store[S2, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _]) ~> (Interface[A1, A1 => B1, _] ⊗ Interface[A2, A2 => B2, _]), Y] =
          (s, a) => (p.update(s, a), (p.readout(s)._1(a._1), p.readout(s)._2(a._2)))
        
  extension [S1, S2, S3, A1, B1, A2, B2, A3, B3, I, O, Y] (p: Moore[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _]])
    def asMealy: Mealy[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _]] =
      new Mealy[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          p.init
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          p.readout
        def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          p.update
        def run[Y]: Run[(Store[S1, _] ⊗ Store[S2, _] ⊗ Store[S3, _]) ~> (Interface[A1, B1, _] ⊗ Interface[A2, B2, _] ⊗ Interface[A3, B3, _]) ~> Interface[I, I => O, _], Y] =
          (s, a) => (p.update(s, a), p.readout(s)(a))

  extension [S1, S2, A1, B1, A2, B2, A3, B3, A4, B4, Y] (p: Moore[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>  BiInterface[A1, A1 => B3, A2, A2 => B4, _]] )
    def asMealy(using R: Runner2[((Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>  BiInterface[A1, A1 => B3, A2, A2 => B4, _]), (S1, S2), A1, B3, A2, B4]): Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>  BiInterface[A1, A1 => B3, A2, A2 => B4, _]] =
      new Mealy[(Store[S1, _] ⊗ Store[S2, _]) ~> (BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _]) ~>  BiInterface[A1, A1 => B3, A2, A2 => B4, _]]:
        def init[Y]: Init[(Store[S1, _] ⊗ Store[S2, _]) ~> ((BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _])) ~> (BiInterface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.init
        def readout[Y]: Readout[(Store[S1, _] ⊗ Store[S2, _]) ~> ((BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _])) ~> (BiInterface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.readout
        def update[Y]: Update[(Store[S1, _] ⊗ Store[S2, _]) ~> ((BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _])) ~> (BiInterface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
          p.update
        def run[Y]: Run[(Store[S1, _] ⊗ Store[S2, _]) ~> ((BiInterface[A1, B1, A2, B2, _] ⊗ BiInterface[A3, B3, A4, B4, _])) ~> (BiInterface[A1, A1 => B3, A2, A2 => B4, _]), Y] =
         p.asPolyMap.run

  extension [S, A1, B1, A2, B2, Y](m: Moore[Store[S, _] ~> (Interface[A1, B1, _] ◁ Interface[A2, A2 => B2, _])])
    @scala.annotation.targetName("asMealyStoreToMonoCompositeMono")
    def asMealy: Mealy[Store[S, _] ~> (Interface[A1, B1, _] ◁ Interface[A2, A2 => B2, _])] =
      new Mealy[Store[S, _] ~> (Interface[A1, B1, _] ◁ Interface[A2, A2 => B2, _])]:
        def init[Y]: Init[(Store[S, _]) ~> ((Interface[A1, B1, _]) ◁ (Interface[A2, A2 => B2, _])), Y] =
          m.init
        def readout[Y]: Readout[(Store[S, _]) ~> ((Interface[A1, B1, _]) ◁ (Interface[A2, A2 => B2, _])), Y] =
          m.readout
        def update[Y]: Update[(Store[S, _]) ~> ((Interface[A1, B1, _]) ◁ (Interface[A2, A2 => B2, _])), Y] =
          m.update
        def run[Y]: Run[(Store[S, _]) ~> ((Interface[A1, B1, _]) ◁ (Interface[A2, A2 => B2, _])), Y] =
          (s, a) => (m.update(s, a), (m.readout._1(s), m.readout._2(m.readout._1(s))(a._2)))
