/**
 * *****************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * *****************************************************************************
 */
package seqset.tests

import org.scalatest._

import seqset.regular._

class ADFATests extends FlatSpec with Matchers {

    "An empty ADFA" should "behave properly" in {
        implicit val dag = ADFA.makeDag[Int]()
        val e = ADFA.empty[Int]
        e.contains(Seq.empty) should be(false)
    }

    "A singleton ADFA" should "behave properly" in {
        implicit val dag = ADFA.makeDag[Int]()
        val s = ADFA[Int](List(1, 2, 3, 4))

        s.contains(Seq(1, 2, 3, 4)) should be(true)
        s.contains(Seq.empty) should be(false)
        s.contains(Seq(1, 2, 3)) should be(false)
        s.contains(Seq(1, 2, 3, 4, 5)) should be(false)
    }

    "Simple ADFA union 1" should "behave properly" in {
        implicit val dag = ADFA.makeDag[Char]()

        val s1 = ADFA[Char]("a")
        val s2 = ADFA[Char]("ab")
        val u = s1 | s2

        u.contains("a") should be(true)
        s2.contains("a") should be(false)
        u.contains("ab") should be(true)
        s1.contains("ab") should be(false)
    }

    "Simple ADFA union 2" should "behave properly" in {
        implicit val dag = ADFA.makeDag[Char]()

        val s1 = ADFA[Char]("ab")
        val s2 = ADFA[Char]("cb")
        val u = s1 | s2

        u.contains("ab") should be(true)
        u.contains("cb") should be(true)
        u.contains("b") should be(false)
        u.contains("") should be(false)
    }

    "Simple ADFA union 3" should "behave properly" in {
        implicit val dag = ADFA.makeDag[Char]()

        val s1 = ADFA[Char]("abcz")
        val s2 = ADFA[Char]("aefz")
        val u = s1 | s2

        u.contains("abcz") should be(true)
        u.contains("aefz") should be(true)
        u.contains("abfz") should be(false)
        u.contains("") should be(false)
    }

    "Simple ADFA concat 1" should "behave properly" in {
        implicit val dag = ADFA.makeDag[Char]()

        val a1 = ADFA[Char]("ab", "a")
        val a2 = ADFA[Char]("cd", "c")
        val c = a1 +++ a2

        c.contains("ab") should be(false)
        c.contains("a") should be(false)
        c.contains("cd") should be(false)
        c.contains("abcd") should be(true)
    }

    "Simple ADFA concat 2" should "behave properly" in {
        implicit val dag = ADFA.makeDag[Char]()

        val a1 = ADFA[Char]("abc", "")
        val a2 = ADFA[Char]("cde")
        val c = a1 +++ a2

        c.contains("abc") should be(false)
        c.contains("abccde") should be(true)
        c.contains("cde") should be(true)
    }

    "ADFA combinations" should "work" in {
        implicit val dag = ADFA.makeDag[Char]()

        val a1 = ADFA("ab")
        val a2 = ADFA("")
        val a3 = ADFA("c", "ce")
        val a4 = a2 | a3
        val a5 = a1 +++ a4
        // val a6 = a5.toDFA
        // val a7 = a6.minimized

        a5.accepts("abce") should be(true)
        // a6.accepts("abce") should be(true)
        // a7.accepts("abce") should be(true)
    }

    "Equality between ADFAs" should "be correct" in {
        implicit val dag = ADFA.makeDag[Char]()

        val a = ADFA("abc", "ab", "abce")
        val b = (ADFA("ab") +++ (ADFA("") | ADFA("c", "ce")))
        val c = ADFA("ab", "abce")

        a.equals(b) should be(true)
        b.equals(a) should be(true)
        a.equals(c) should be(false)
        c.equals(a) should be(false)
        b.equals(c) should be(false)
        c.equals(b) should be(false)
    }
}
