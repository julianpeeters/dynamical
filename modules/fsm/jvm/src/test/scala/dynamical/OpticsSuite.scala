package dynamical

import cats.implicits.given
import dynamical.fsm.{Mealy, Moore, Wiring}
import munit.FunSuite
import polynomial.morphism.~>
import polynomial.`object`.Monomial.{Interface, Store}

class OpticsSuite extends FunSuite: // See https://www.scala-exercises.org/monocle

    case class Address(strNumber: Int, streetName: String)
    case class Person(name: String, age: Int, address: Address)

    sealed trait Json
    case object JNull extends Json
    case class JStr(v: String) extends Json
    case class JNum(v: Double) extends Json
    case class JObj(v: Map[String, Json]) extends Json

    case class Point(id: String, x: Int, y: Int)

    test("address"):
      val m: Mealy[Store[Address, _] ~> Interface[Int, Int => Int, _]] =
        Mealy(Address(10, "High Street"), s => _ => s.strNumber, (s, i) => s.copy(strNumber = i))
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[Int] = List(10, 1, 2)
      assertEquals(obtained, expected)

    test("person's street number"):
      val l: Moore[Store[Person, _] ~> Interface[Address, Address, _]] = Moore(Person("John", 20, Address(10, "High Street")), s => s.address, (s, i) => s.copy(address = i))
      def w[Y]: Wiring[Interface[Address, Address, _] ~> Interface[Int, Int => Int, _]] = Wiring(i => j => i.strNumber, (i1, i2) => i1.copy(strNumber = i2))
      val m: Mealy[Store[Person, _] ~> Interface[Address, Address, _] ~> Interface[Int, Int => Int, _]] = l.andThen(w).asMealy        
      val obtained: List[Int] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[Int] = List(10, 1, 2)
      assertEquals(obtained, expected)

    test("str miss"):
      val m: Mealy[Store[Json, _] ~> Interface[String, String => Option[String], _]] =
        Mealy(JNull, s => _ => s match
          case JNull => None
          case JStr(v) => Some(v)
          case JNum(v) => None
          case JObj(v) => None
        , (s, i) => s match
          case JNull => s
          case JStr(v) => JStr(i)
          case JNum(v) => s
          case JObj(v) => s
          )
      val obtained: List[Option[String]] = List("hello", "howdy", "hi").mapAccumulate(m.init)(m.run)._2
      val expected: List[Option[String]] = List(None, None, None)
      assertEquals(obtained, expected)

    test("str hit"):
      val m: Mealy[Store[Json, _] ~> Interface[String, String => Option[String], _]] =
        Mealy(JStr("good day"), s => _ => s match
          case JNull => None
          case JStr(v) => Some(v)
          case JNum(v) => None
          case JObj(v) => None
        , (s, i) => s match
          case JNull => s
          case JStr(v) => JStr(i)
          case JNum(v) => s
          case JObj(v) => s
          )
      val obtained: List[Option[String]] = List("hello", "howdy", "hi").mapAccumulate(m.init)(m.run)._2
      val expected: List[Option[String]] = List(Some("good day"), Some("hello"), Some("howdy"))
      assertEquals(obtained, expected)

    test("list head"):
      val m: Mealy[Store[List[Int], _] ~> Interface[Int, Int => Option[Int], _]] =
        Mealy(List(10), s => _ => s.headOption, (s, i) => i +: s)
      val obtained: List[Option[Int]] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[Option[Int]] = List(Some(10), Some(1), Some(2))
      assertEquals(obtained, expected)

    test("list all"):
      val m: Mealy[Store[List[Int], _] ~> Interface[Int, Int => List[Long], _]] =
        Mealy(List(10, 20, 30), s => _ => s.map(_.toLong), (s, i) => s.map(v => i * v))
      val obtained: List[List[Long]] = List(1, 2, 3).mapAccumulate(m.init)(m.run)._2
      val expected: List[List[Long]] = List(List(10, 20, 30), List(10, 20, 30), List(20, 40, 60))
      assertEquals(obtained, expected)

    test("flip point"):
      val m: Mealy[Store[Point, _] ~> Interface[(Int, Int), ((Int, Int)) => Point, _]] =
        Mealy[Point, (Int, Int), Point, Nothing](Point("bottom left", 0, 0), s => _ => s, (s, i: (Int, Int)) => Point(s.id, i._1, i._2))
      val obtained: List[Point] = List((1, 0), (0, 1)).mapAccumulate(m.init)(m.run)._2
      val expected: List[Point] = List(Point("bottom left", 0, 0), Point("bottom left", 1, 0))
      assertEquals(obtained, expected)