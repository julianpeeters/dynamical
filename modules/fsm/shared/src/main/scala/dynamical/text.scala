package dynamical

import cats.implicits.given
import dynamical.fsm.{Moore, Wiring}
import dynamical.seq.noneTerminate
import polynomial.morphism.~>
import polynomial.`object`.{Binomial, Store}

object text:

  def lineReader: Moore[Store[List[String], _] ~> Binomial[Some[String], None.type, None.type, Some[List[String]], _]] =
    Moore(List.empty[String], _ => None, s => Some(s), (s, i) => s ++ i.value.split("\n"), (s, i) => s)

  val wrappedLineReader: Moore[
    Store[List[String], _] ~>
      Binomial[Some[String], None.type, None.type, Some[List[String]], _] ~>
        Binomial[Some[String], Some[String] => None.type, None.type, None.type => Some[List[String]], _]
  ] = lineReader.andThen(
    Wiring(
      none => _ => none,
      some => _ => some,
      (none, a1) => _ => none,
      (some, a2) => _ => some
    )
  )

  def lines: Seq[String] => Seq[String] =
    strings =>
      strings
        .noneTerminate
        .mapAccumulate[List[String], Option[List[String]]](wrappedLineReader.init)((s, a) =>
          a match
            case v@Some(value) => (wrappedLineReader.update._1(s, v), wrappedLineReader.readout._1(s)(v))
            case None => (wrappedLineReader.update._2(s, None), wrappedLineReader.readout._2(s)(None))
        )._2.foldLeft(Seq.empty[String])((acc, ms) => if ms.isDefined then acc ++ ms.get else acc)

  object utf8:

    def decoder: Moore[Store[String, _] ~> Binomial[Some[Byte], None.type, None.type, Some[String], _]] =
      Moore(
        "",
        _ => None,
        s => Some(String(s)),
        (s, i) => s + i.value.toChar,
        (s, _) => s
      )

    val wrappedDecoder: Moore[
      Store[String, _] ~>
        Binomial[Some[Byte], None.type, None.type, Some[String], _] ~>
          Binomial[Some[Byte], Some[Byte] => None.type, None.type, None.type => Some[String], _]
    ] =
      decoder.andThen(
        Wiring(
          none => _ => none,
          some => _ => some,
          (none, a1) => _ => none,
          (some, a2) => _ => some
        )
      )

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