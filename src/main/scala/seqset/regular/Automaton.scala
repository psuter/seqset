/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package seqset
package regular

trait Automaton[A] extends SeqSet[A] {
  override final def contains(s: Seq[A]) = accepts(s)

  def accepts(s: Seq[A]) : Boolean

  def toDFA : DFA[A]

  def toNFA : NFA[A]

  override final def hashCode() : Int = {
    //throw new Exception("You really shouldn't be taking the hash of an automaton.")
    0
  }

  def +(s: Seq[A]) : Automaton[A] = this | DFA(s)

  def |(other: Automaton[A]) : Automaton[A] = {
    (this.toNFA union other.toNFA).toDFA.minimized
  }

  def +++(other: Automaton[A]) : Automaton[A] = {
    (this.toNFA concat other.toNFA).toDFA.minimized
  }

  def tails(head: A) : Automaton[A] = this.toDFA.tails(head)

  override def iterator : Iterator[Seq[A]] = this.toDFA.iterator

  // FIXME remove method with ugly name
  def postfixes(head: A) = tails(head)
}

object Automaton {
  def empty[A] : Automaton[A] = DFA.empty[A]
}
