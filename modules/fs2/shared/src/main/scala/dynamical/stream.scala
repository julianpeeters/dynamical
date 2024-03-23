package dynamical.stream

import cats.effect.kernel.Ref
import cats.effect.IO
import fs2.Pipe
import dynamical.fsm.Mealy
import dynamical.fsm.methods.types.Unify2
import polynomial.morphism.~>
import polynomial.`object`.Binomial.BiInterface
import polynomial.`object`.Monomial.{Interface, Store, StoreF}

extension [S, A, B] (m: Mealy[Store[S, _] ~> Interface[A, A => B, _]])
  @scala.annotation.targetName("transducerStoreToMono")
  def transducer[F[_], Y]: Pipe[F, A, B] =
    stream =>
      stream
        .mapAccumulate(m.init)(m.run)
        .map(_._2)

extension [S, A, B] (m: Mealy[Store[S, _] ~> Interface[A, B, _] ~> Interface[A, A => B, _]])
  @scala.annotation.targetName("transducerStoreToMonoToMono")
  def transducer[F[_], Y]: Pipe[F, A, B] =
    stream =>
      stream
        .mapAccumulate(m.init)(m.run)
        .map(_._2)

extension [S, A, B] (m: Mealy[(StoreF[IO, Ref[IO, Boolean], _] ~> Interface[A, IO[B], _]) ~> Interface[A, A => IO[B], _]])
  @scala.annotation.targetName("transducerFStoreToMonoToMono")
  def transducer[Y]: Pipe[IO, A, B] =
    stream =>
      stream
        .evalMapAccumulate(m.init)((s, i) => m.run(s, i))
        .map(_._2)

extension [S, A1, B1, A2, B2] (m: Mealy[Store[S, _] ~> BiInterface[A1, A1 => B1, A2, A2 => B2, _]])
  @scala.annotation.targetName("transducerStoreToBi")
  def transducer[F[_], Y]: Pipe[F, Unify2[A1, A2], Unify2[B1, B2]] =
    stream =>
      stream
        .mapAccumulate(m.init)(m.run)
        .map(_._2)

