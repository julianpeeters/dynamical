package dynamical.seq

case class EOS(msg: String)

extension [A, B] (as: Seq[A])

  def through(p: Seq[A] => Seq[B]): Seq[B] =
    p(as)

  def noneTerminate: Seq[Option[A]] =
    as.map(v => Some(v)) :+ None