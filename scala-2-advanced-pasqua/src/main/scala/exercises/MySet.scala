package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  /*
  Exercise - implement a functional set
   */
  def apply(elem: A): Boolean = contains(elem)

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def forEach(f: A => Unit): Unit
}

class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new NonEmptySet(elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def forEach(f: A => Unit): Unit = ()
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  override def +(elem: A): MySet[A] =
    if (this contains (elem)) this
    else new NonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = {
    tail ++ anotherSet + head
    /*
    [1 2 3] ++ [4 5] =
    [2 3] ++ [4 5] + 1
    [3] ++ [4 5] + 1 +2
    [] ++ [4 5] + 1 + 2 + 3
    [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
     */
  }

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def forEach(f: A => Unit): Unit = {
    f(head)
    tail forEach f
  }
}


object MySet {
  /*
    val s = MySet(1, 2, 3) = buildSet(seq(1,2,3), [])
    = buildSet(seq(2,3), [], +1)
    = buildSet(seq(3), [1], +2)
    = buildSet(seq(), [1, 2], +3)
    = [1, 2, 3]
   */
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Set[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSet, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0) forEach println

}
