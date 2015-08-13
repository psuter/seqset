package seqset

trait SeqSet[A] {
  def contains(s: Seq[A]) : Boolean

  def iterator : Iterator[Seq[A]]
}
