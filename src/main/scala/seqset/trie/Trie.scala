/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package seqset
package trie

import scala.annotation.tailrec

// An immutable datastructure for sets of sequences of A.
sealed trait Trie[A] extends SeqSet[A] {
  // Whether the trie contains any sequence.
  def isEmpty : Boolean

  // The number of distinct sequences contained in the trie.
  def size : Int

  // Computes a new trie which additionnally includes `s`.
  def +(s: Seq[A]) : Trie[A]

  // Computes a trie representing the product of all concatenations.
  def +++(t: Trie[A]) : Trie[A]

  // Computes the union of two tries.
  def |(t: Trie[A]) : Trie[A]

  // Checks whether a sequence is contained in the Trie.
  def contains(s: Seq[A]) : Boolean

  // Returns a trie encoding all the postfixes for a given prefix.
  def postfixes(prefix: A): Trie[A]

  // Returns all heads for which there exist at least one sequence in the trie.
  // (Will not tell you whether the empty sequence is contained.)
  def heads : Set[A]

  // An iterator returning all sequences encoded in thie Trie.
  def iterator : Iterator[Seq[A]]

  // The number of distinct nodes in the trie (aka. the number of distinct prefixes).
  def nodeCount : Int
}

object Trie {
  def empty[A] : Trie[A] = emptyNode[A]

  private def emptyNode[A] = new Node[A](Map.empty[A,Node[A]], isTerminal = false)

  private class Node[A](val map: Map[A,Node[A]], val isTerminal: Boolean) extends Trie[A] {
    // Note that this assumes we never have "dangling nodes".
    // Safe as long we don't remove from a Trie.
    val isEmpty : Boolean = !isTerminal && map.isEmpty

    def size : Int = map.values.foldLeft(0)(_ + _.size) + (if(isTerminal) 1 else 0)

    def nodeCount : Int = map.values.foldLeft(0)(_ + _.nodeCount) + 1

    def heads : Set[A] = map.keySet

    def postfixes(prefix: A) : Trie[A] = map.getOrElse(prefix, emptyNode[A])
  
    @tailrec
    final def contains(s: Seq[A]) : Boolean = if(s.isEmpty) {
      isTerminal
    } else {
      val sub: Option[Node[A]] = map.get(s.head)
  
      if(sub.isDefined) {
        sub.get.contains(s.tail)
      } else {
        false
      }
    }
  
    def +(s: Seq[A]) : Node[A] = {
      if(s.isEmpty) {
        if(isTerminal) {
          this
        } else {
          new Node(map, true)
        }
      } else {
        val h +: t = s
        val sub: Node[A] = map.getOrElse(h, emptyNode[A])
        val newSub = sub + t
        new Node(map.updated(h, newSub), isTerminal)      
      }
    }
  
    def +++(t: Trie[A]) : Node[A] = {
      val withPostfixes = new Node(map.mapValues(_ +++ t), false)

      if(isTerminal) {
        withPostfixes | t
      } else {
        withPostfixes
      }
    }

    def |(t: Trie[A]) : Node[A] = {
      // FIXME ugly
      val that: Node[A] = t.asInstanceOf[Node[A]]

      if(this.eq(t)) {
        this
      } else if(this.isEmpty) {
        that
      } else if(that.isEmpty) {
        this
      } else {
        val (m1, t1) = (this.map, this.isTerminal)
        val (m2, t2) = (that.map, that.isTerminal)

        val newKeys : Seq[A] = (m1.keySet ++ m2.keySet).toSeq
        val newMap : Map[A,Node[A]] = newKeys.map { k =>
          val v = (m1.get(k), m2.get(k)) match {
            case (Some(v1), None)     => v1
            case (None, Some(v2))     => v2
            case (Some(v1), Some(v2)) => v1 | v2
            case (None,None)          => assert(false); ???
          }

          (k -> v)
        }.toMap

        new Node(newMap, t1 | t2)
      }
    }

    def iterator : Iterator[Seq[A]] = {
      val base: Iterator[Seq[A]] = map.toIterator.flatMap { p =>
        p._2.iterator.map(t => p._1 +: t) 
      }

      if(isTerminal) {
        Seq(Seq.empty[A]).toIterator ++ base
      } else {
        base
      }
    }

    override def equals(that: Any) : Boolean = {
      if(that == null) {
        false
      } else if(that.isInstanceOf[AnyRef] && that.asInstanceOf[AnyRef].eq(this)) {
        true
      } else if(!that.isInstanceOf[Node[_]]) {
        false
      } else {
        val other = that.asInstanceOf[Node[_]]
        other.isTerminal == this.isTerminal && other.map == this.map
      }
    }

    override lazy val hashCode : Int = (map, isTerminal).hashCode
  }
}
