package dynamical.stream

import fs2.Pipe
import dynamical.fsm.Mealy
import dynamical.fsm.methods.types.Unify2
import polynomial.morphism.~>
import polynomial.`object`.{Bi, Mono}

extension [S, A, B] (m: Mealy[Mono.Store[S, _] ~> Mono.Interface[A, A => B, _]])
  @scala.annotation.targetName("transducerStoreToMono")
  def transducer[F[_], Y]: Pipe[F, A, B] =
    stream =>
      stream
        .mapAccumulate(m.init)(m.run)
        .map(_._2)

extension [S, A1, B1, A2, B2] (m: Mealy[Mono.Store[S, _] ~> Bi.Interface[A1, A1 => B1, A2, A2 => B2, _]])
  @scala.annotation.targetName("transducerStoreToBi")
  def transducer[F[_], Y]: Pipe[F, Unify2[A1, A2], Unify2[B1, B2]] =
    stream =>
      stream
        .mapAccumulate(m.init)(m.run)
        .map(_._2)

