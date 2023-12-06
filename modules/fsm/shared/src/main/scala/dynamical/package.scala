package dynamical

implicit class SeqOps[A, B](as: Seq[A]):

  def through(p: Seq[A] => Seq[B]): Seq[B] =
    p(as)

  def noneTerminate: Seq[Option[A]] =
    as.map(v => Some(v)) :+ None
