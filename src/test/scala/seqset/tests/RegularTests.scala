/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package seqset.tests

import org.scalatest._

import seqset.regular._

class RegularTests extends FlatSpec with Matchers {

  val s1 : Seq[Seq[Char]] = Seq(
    "hello",
    "helicopter",
    "hallo",
    "halicopter",
    "mango"
  )

  val a1 : DFA[Char] = DFA(s1 : _*)
  val m1 : DFA[Char] = a1.minimized

  "An empty automaton" should "behave properly" in {
    val e = DFA.empty[Int]

    e.stateCount should be (1)
    e.minimized.stateCount should be (1) 

    e.toNFA.toDFA.stateCount should be (1)
  }

  "A multi-sequence automaton" should "behave properly" in {
    a1.stateCount should be (29)
    for(s <- s1) { a1.contains(s) should be (true) }
    a1.contains("helacopter") should be (false)

    m1.stateCount should be (15)
    for(s <- s1) { m1.contains(s) should be (true) }
    m1.contains("helacopter") should be (false)
  }

  "This" should "work" in {
    val s1 : Seq[Seq[Char]] = Seq("hello", "hallo")
    val s2 : Seq[Seq[Char]] = Seq("heiko", "dagger")

    val a1 = DFA(s1 : _*)
    val a2 = DFA(s2 : _*)

    val n1 = a1.toNFA

    for(s <- s1) {
      a1.contains(s) should be (true)
      n1.contains(s) should be (true)
    }

    val a = a1 | a2

    for(s <- (s1 ++ s2)) {
      a.contains(s) should be (true)
    }
    a.contains("heilo") should be (false)

    val b = a1 +++ a2
    b.contains("helloheiko") should be (true)
    b.contains("hallodagger") should be (true)
    b.contains("") should be (false)
    b.contains("hellohallo") should be (false)

    b.tails('h').contains("allodagger") should be (true)

    val cs: Set[String] = b.iterator.toSet.map((s : Seq[Char]) => s.mkString)
    cs should be (Set("helloheiko", "halloheiko", "hellodagger", "hallodagger"))
  }

  "Automata combinations" should "work" in {
    val a1 = DFA("ab")
    val a2 = DFA("")
    val a3 = DFA("c", "ce")
    val a4 = a2 | a3
    val a5 = a1 +++ a4
    val a6 = a5.toDFA
    val a7 = a6.minimized

    a5.accepts("abce") should be (true)
    a6.accepts("abce") should be (true)
    a7.accepts("abce") should be (true)
  }

  "Equality between automata" should "be correct" in {
    val a = DFA("abc", "ab", "abce")
    val b = (DFA("ab") +++ (DFA("") | DFA("c", "ce"))).toDFA
    val c = DFA("ab", "abce")

    a.equals(b) should be (true) 
    b.equals(a) should be (true) 
    a.equals(c) should be (false) 
    c.equals(a) should be (false) 
    b.equals(c) should be (false) 
    c.equals(b) should be (false) 
  }
}
