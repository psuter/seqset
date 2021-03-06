/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package seqset.tests

import org.scalatest._

import seqset.trie._

class TrieTests extends FlatSpec with Matchers {
  // I'll happily admit that I don't know how to write proper, pretty, BDD-like, tests in ScalaTest.

  "An empty Trie" should "behave properly" in {
    val t = Trie.empty[String]
  
    t.isEmpty should be (true)
    t.size should be (0)
    t.nodeCount should be (1)
    t.contains(Seq("")) should be (false)
    t.iterator.isEmpty should be (true)
  }

  "A Trie containing a single sequence" should "behave properly" in {
    val s1 : Seq[Int] = List(1, 2, 3, 4)
    val s2 : Seq[Int] = (1 to 4).toList
    val s3 : Seq[Int] = List(1, 2, 3)
    val s4 : Seq[Int] = List(1, 2, 3, 4, 5)
    val s5 : Seq[Int] = Nil

    val t : Trie[Int] = Trie.empty[Int] + s1

    t.contains(s1) should be (true)
    t.contains(s2) should be (true)
    t.contains(s3) should be (false)
    t.contains(s4) should be (false)
    t.contains(s5) should be (false)

    t.isEmpty should be (false)

    t.size should be (1)

    t.nodeCount should be (5)

    t.iterator.toSet should be (Set(Seq(1,2,3,4)))
  }

  "A Trie containing multiple sequences" should "behave properly" in {
    val s1 : Seq[String] = List("one", "two", "three")
    val s2 : Seq[String] = List("one", "two")
    val s3 : Seq[String] = List("a", "b", "three")

    val t : Trie[String] = Trie.empty[String] + s1 + s2 + s3

    t.contains(s1) should be (true)
    t.contains(s2) should be (true)
    t.contains(s3) should be (true)

    t.contains(s1.reverse) should be (false)
    t.contains(List("one")) should be (false)

    t.isEmpty should be (false)

    t.size should be (3)

    (t + s1).size should be (3)

    t.nodeCount should be (7)

    t.iterator.toSet should be (Set(s1,s2,s3))
  }

  "The union of two Tries" should "behave properly" in {
    val s1: Seq[Int] = List(1, 2, 3)
    val s2: Seq[Int] = List(1, 2, 4)
    val s3: Seq[Int] = List(1, 3, 5)
    val s4: Seq[Int] = List(4, 7, 6)
    val s5: Seq[Int] = List(1, 2, 5)

    val t1: Trie[Int] = Trie.empty[Int] + s1 + s2
    val t2: Trie[Int] = Trie.empty[Int] + s3 + s4 + s2

    val u = t1 | t2

    u.contains(s1) should be (true)
    u.contains(s2) should be (true)
    u.contains(s3) should be (true)
    u.contains(s4) should be (true)
    u.contains(s5) should be (false)
    u.contains(Nil) should be (false)

    u.size should be (4)
    u.nodeCount should be (10)
  }

  "The concatenation of two Tries" should "behave properly" in {
    val s1: Seq[Char] = "Hello"
    val s2: Seq[Char] = "Helicopter"
    val s3: Seq[Char] = "World"
    val s4: Seq[Char] = "Propeller"

    val t1: Trie[Char] = Trie.empty[Char] + s1 + s2
    val t2: Trie[Char] = Trie.empty[Char] + s3 + s4

    val c = t1 +++ t2

    c.contains(s1) should be (false)
    c.contains(s2) should be (false)
    c.contains(s3) should be (false)
    c.contains(s4) should be (false)

    c.contains("HelloWorld") should be (true)
    c.contains("HelicopterPropeller") should be (true)

    val valueSet : Set[Seq[Char]] = c.iterator.toSet
    valueSet.map(_.mkString("")) should be (Set("HelloWorld", "HelloPropeller", "HelicopterWorld", "HelicopterPropeller"))
  }

  "Postfixes of a Trie" should "behave properly" in {
    val t = Trie.empty[Int] +
      List(1, 2, 3, 4, 5, 6, 7) +
      List(1, 3, 5, 7, 9, 11) +
      List(2, 4, 6, 8, 10) +
      List(1, 4, 9, 25, 36) +
      List(2)

    val s0 = t.postfixes(0)
    val s1 = t.postfixes(1)
    val s2 = t.postfixes(2)

    s0.size should be (0)
    s1.size should be (3)
    s2.size should be (2)

    s1.contains(List(4, 9, 25, 36)) should be (true)
    s1.contains(List(1, 3, 5, 7, 9, 11)) should be (false)

    s2.contains(List.empty[Int]) should be (true)
  }

  "Union with self" should "not create a new Trie" in {
    val t1 = Trie.empty[Char] + "Hello" + "Helicopter" + "World"
    val t2 = Trie.empty[Char] + "World" + "Helicopter" + "Hello"

    val s1 = t1 | t1
    val s2 = t1 | t2

    (s1 == t1) should be (true)
    s1.eq(t1) should be (true)

    (s2 == t1) should be (true)
    s2.eq(t1) should be (false) // it could be true, but that would be suspicious
  }
}
