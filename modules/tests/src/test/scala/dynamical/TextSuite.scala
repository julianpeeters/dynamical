package dynamical

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