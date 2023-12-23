package dynamical.seq

case object EOS

extension [A] (as: Seq[A])

  def through[B](p: Seq[A] => Seq[B]): Seq[B] =
    p(as)

  def noneTerminate: Seq[Option[A]] =
    as.map(v => Some(v)) :+ None