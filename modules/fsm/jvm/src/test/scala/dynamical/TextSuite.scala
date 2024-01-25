package dynamical

import cats.implicits.given
// import destructured.given
import dynamical.fsm.Wiring
import dynamical.seq.{noneTerminate2, through}

import munit.FunSuite

class TextSuite extends FunSuite:

  test("decode"):
    val input: Seq[Byte] = "hello".getBytes().toSeq
    val obtained: Seq[String] = input.through(text.utf8.decode)
    val expected: Seq[String] = Seq("hello")
    assertEquals(obtained, expected)

  test("lines"):
    val input: Seq[Byte] = "hello\nworld".getBytes().toSeq
    val obtained: Seq[String] = input.through(text.utf8.decode).through(text.lines)
    val expected: Seq[String] = Seq("hello", "world")
    assertEquals(obtained, expected)


  test("text tensor"):
    val machine = (text.utf8.decoder ⊗ text.lineReader.swapInterfacePos).andThen(Wiring.serially).asMealy
    val obtained = "hello\ngoodbye".getBytes().toList.noneTerminate2.mapAccumulate(machine.init)(machine.run)._2
      .foldLeft(Seq.empty[String])((acc, ms) => if ms.isDefined then acc ++ ms.get else acc).toList
    val expected: List[String] = List("hello", "goodbye")
    assertEquals(obtained, expected)