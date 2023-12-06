package dynamical

import cats.implicits.*
import dynamical.fsm.Mealy
import polynomial.morphism.~>
import polynomial.`object`.{Binomial, Store}

object text:
    
  def lines: Seq[String] => Seq[String] =
    strings =>
      val m: Mealy[Store[List[String], _] ~> Binomial[Some[String], Some[String] => None.type, None.type, None.type => Some[List[String]], _]] =
        Mealy(List.empty[String], s => _ => None, s => _ => Some(s), (s, i) => s ++ i.value.split("\n"), (s, i) => List.empty[String])
      strings.noneTerminate.mapAccumulate(m.init)(m.run)._2.foldLeft(Seq.empty[String])((acc, ms) => ms.fold(acc)(s => acc ++ s))

  object utf8:
          
    def decode: Seq[Byte] => Seq[String] =
      bytes =>
        val m: Mealy[Store[Array[Byte], _] ~> Binomial[Some[Byte], Some[Byte] => None.type, None.type, None.type => Some[String], _]] =
          Mealy(Array.emptyByteArray, s => _ => None, s => _ => Some(String(s)), (s, i) => s :+ i.value, (s, i) => Array.emptyByteArray)
        bytes.noneTerminate.mapAccumulate(m.init)(m.run)._2.foldLeft(Seq.empty[String])((acc, ms) => ms.fold(acc)(s => acc :+ s))