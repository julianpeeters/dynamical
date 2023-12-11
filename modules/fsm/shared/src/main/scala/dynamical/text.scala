package dynamical

import cats.implicits.*
import dynamical.fsm.{Moore, Wrapper}
import polynomial.morphism.~>
import polynomial.`object`.{Binomial, Store}

object text:

  val reader: Moore[Store[List[String], _] ~> Binomial[Some[String], None.type, None.type, Some[List[String]], _]] =
    Moore(List.empty[String], _ => None, s => Some(s), (s, i) => s ++ i.value.split("\n"), (s, i) => List.empty[String])

  val wrappedReader: Moore[
    Store[List[String], _] ~>
      Binomial[Some[String], None.type, None.type, Some[List[String]], _] ~>
        Binomial[Some[String], Some[String] => None.type, None.type, None.type => Some[List[String]], _]
  ] = reader.andThen(Wrapper(none => _ => None, some => _ => some, none => _ => None, some => _ => some))

  def lines: Seq[String] => Seq[String] =
    strings =>
      strings
        .noneTerminate
        .mapAccumulate[List[String], Option[List[String]]](wrappedReader.init)((s, a) =>
          a match
            case v@Some(value) => (wrappedReader.update._1(s, v), wrappedReader.readout._1(s)(v))
            case None => (wrappedReader.update._2(s, None), wrappedReader.readout._2(s)(None))
        )._2.foldLeft(Seq.empty[String])((acc, ms) => if ms.isDefined then acc ++ ms.get else acc)

  object utf8:

    val decoder: Moore[Store[Array[Byte], _] ~> Binomial[Some[Byte], None.type, None.type, Some[String], _]] =
      Moore(
        Array.emptyByteArray,
        _ => None,
        s => Some(String(s)),
        (s, i) => s :+ i.value,
        (s, i) => Array.emptyByteArray
      )

    val wrappedDecoder: Moore[Store[Array[Byte], _] ~> Binomial[Some[Byte], None.type, None.type, Some[String], _] ~> Binomial[Some[Byte], Some[Byte] => None.type, None.type, None.type => Some[String], _]] =
      decoder.andThen(Wrapper(none => _ => None, some => _ => some, some => _ => None, some => _ => some))
          
    def decode: Seq[Byte] => Seq[String] =
      bytes =>
        bytes
          .noneTerminate
          .mapAccumulate(decoder.init)(
            (s, a) =>
          a match
            case v@Some(value) => (wrappedDecoder.update._1(s, v), wrappedDecoder.readout._1(s)(v))
            case None => (wrappedDecoder.update._2(s, None), wrappedDecoder.readout._2(s)(None))
        )._2.foldLeft(Seq.empty[String])((acc, ms) => if ms.isDefined then acc :+ ms.get else acc)