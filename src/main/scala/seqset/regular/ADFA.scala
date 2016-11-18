/**
 * *****************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * *****************************************************************************
 */
package seqset
package regular

import scala.annotation.tailrec

import scala.collection.immutable.BitSet

import play.api.libs.json._

class ADFA[A] protected[regular] (private val dagIndex: Int)(implicit private val dag: ADFADag[A]) extends Automaton[A] {
    def accepts(s: Seq[A]): Boolean = dag.accepts(dagIndex, s)

    def toDFA: DFA[A] = ADFA.convertToDFA[A](this)

    def toNFA: NFA[A] = this.toDFA.toNFA

    override def +(s: Seq[A]): ADFA[A] = {
        val other = ADFA[A](Seq(s): _*)
        ADFA.union[A](this, other)
    }

    override def |(other: Automaton[A]): Automaton[A] = {
        require(other != null)
        require(other.isInstanceOf[ADFA[_]])
        val o = other.asInstanceOf[ADFA[A]]
        require(this.dag eq o.dag)
        ADFA.union[A](this, o)
    }

    override def +++(other: Automaton[A]): Automaton[A] = {
        require(other != null)
        require(other.isInstanceOf[ADFA[_]])
        val o = other.asInstanceOf[ADFA[A]]
        require(this.dag eq o.dag)
        ADFA.concat[A](this, o)
    }

    override def tails(head: A): Automaton[A] = ???

    override def iterator: Iterator[Seq[A]] = ???

    override def filterHeads(f: A => Boolean): Automaton[A] = this.toDFA.filterHeads(f)

    override def equals(other: Any): Boolean = {
        (other != null && other.isInstanceOf[ADFA[_]] && {
            val o = other.asInstanceOf[ADFA[A]]
            (this.dag eq o.dag) && (this.dagIndex == o.dagIndex)
        })
    }
}

// Stores a hash-cons'd DAG.
class ADFADag[A]() {
    import scala.collection.mutable.{ Map => MutableMap }
    import scala.collection.mutable.ArrayBuffer

    private def checkStateIndex(i: Int): Unit = {
        assert(i >= 0 && i < indexToState.length)
    }

    // Unique index for all "characters", so that all other structures can use strings.
    private val charToIndex = MutableMap.empty[A, Int]
    protected[regular] val indexToChar = ArrayBuffer.empty[A]
    protected[regular] def lookupChar(c: A): Int = {
        charToIndex.getOrElse(c, this.synchronized {
            indexToChar.append(c)
            val i = indexToChar.length - 1
            charToIndex(c) = i
            i
        })
    }

    // A forward map of states is implemented as a (sorted) list of index pairs;
    // first index is an index represents the char, second index represents the destination state.
    protected[regular] case class StateDef(accepting: Boolean, forward: List[(Int, Int)]) {
        def follow(charIndex: Int): Option[Int] = forward.find(p => p._1 == charIndex).map(_._2)
    }

    private val stateToIndex = MutableMap.empty[StateDef, Int]
    protected[regular] val indexToState = ArrayBuffer.empty[StateDef]
    protected[regular] def lookupState(s: StateDef): Int = {
        stateToIndex.getOrElse(s, this.synchronized {
            indexToState.append(s)
            val i = indexToState.length - 1
            stateToIndex(s) = i
            i
        })
    }

    protected[regular] val finalRejecting = lookupState(StateDef(false, Nil))
    protected[regular] val finalAccepting = lookupState(StateDef(true, Nil))

    protected[regular] def accepts(from: Int, seq: Seq[A]): Boolean = {
        checkStateIndex(from)
        val cis = seq.flatMap(c => charToIndex.get(c))
        if (cis.length < seq.length) {
            false // Means at least one of the characters was never seen, hence cannot be accepted.
        } else {
            accepts0(from, cis)
        }
    }

    @tailrec
    private def accepts0(from: Int, charIndices: Seq[Int]): Boolean = {
        val s = indexToState(from)
        if (charIndices.isEmpty) {
            s.accepting
        } else {
            val n = s.follow(charIndices.head)
            if (n.isDefined) {
                accepts0(n.get, charIndices.tail)
            } else {
                false
            }
        }
    }
}

object ADFA {
    def makeDag[A](): ADFADag[A] = {
        new ADFADag[A]()
    }

    def empty[A](implicit dag: ADFADag[A]): ADFA[A] = {
        new ADFA(dag.finalRejecting)
    }

    private def singleton[A](seq: Seq[A])(implicit dag: ADFADag[A]): ADFA[A] = {
        seq.foldRight[ADFA[A]](new ADFA(dag.finalAccepting)) { (a, adfa) =>
            val charIndex = dag.lookupChar(a)
            val stateIndex = dag.lookupState(dag.StateDef(accepting = false, List(charIndex -> adfa.dagIndex)))
            new ADFA[A](stateIndex)
        }
    }

    def apply[A](seqs: Seq[A]*)(implicit dag: ADFADag[A]): ADFA[A] = {
        seqs.foldLeft[ADFA[A]](empty[A]) { (a, s) =>
            val sa = singleton[A](s)
            ADFA.union[A](a, sa)
        }
    }

    def union[A](a1: ADFA[A], a2: ADFA[A]): ADFA[A] = {
        require(a1.dag eq a2.dag)

        implicit val dag = a1.dag

        if (a1.dagIndex == a2.dagIndex) {
            return a1
        }

        if (a1.dagIndex == dag.finalRejecting) {
            return a2
        }

        if (a2.dagIndex == dag.finalRejecting) {
            return a1
        }

        val dag.StateDef(acc1, fwd1) = dag.indexToState(a1.dagIndex)
        val dag.StateDef(acc2, fwd2) = dag.indexToState(a2.dagIndex)

        val newKeys = (fwd1.map(_._1) ++ fwd2.map(_._1)).sorted.distinct

        val fwd1Map = fwd1.toMap
        val fwd2Map = fwd2.toMap

        val newFwd: List[(Int, Int)] = newKeys.map(k => {
            (fwd1Map.get(k), fwd2Map.get(k)) match {
                case (Some(d), None) => (k -> d)
                case (None, Some(d)) => (k -> d)
                case (Some(d1), Some(d2)) =>
                    val rec = ADFA.union(new ADFA[A](d1), new ADFA[A](d2))
                    (k -> rec.dagIndex)
                case _ => assert(false); ??? // can't happen.
            }
        })

        val newStateDef = dag.StateDef(acc1 || acc2, newFwd)
        new ADFA[A](dag.lookupState(newStateDef))
    }

    def concat[A](a1: ADFA[A], a2: ADFA[A]): ADFA[A] = {
        require(a1.dag eq a2.dag)

        implicit val dag = a1.dag

        val dag.StateDef(acc1, fwd1) = dag.indexToState(a1.dagIndex)
        val dag.StateDef(acc2, fwd2) = dag.indexToState(a2.dagIndex)

        val newKeys = fwd1.map({
            case (k, v) =>
                val vIsAccepting = dag.indexToState(v).accepting

                val aSub = new ADFA[A](v)
                val rec = ADFA.concat[A](aSub, a2)

                if (vIsAccepting) {
                    (k -> ADFA.union(a2, rec).dagIndex)
                } else {
                    (k -> rec.dagIndex)
                }
        })

        val withoutAcc = new ADFA[A](dag.lookupState(dag.StateDef(false, newKeys)))
        if (acc1) {
            ADFA.union(a2, withoutAcc)
        } else {
            withoutAcc
        }
    }

    def convertToDFA[A](a: ADFA[A]): DFA[A] = {
        import scala.collection.mutable.{ Set => MutableSet }
        import scala.collection.mutable.{ Map => MutableMap }

        val dag = a.dag

        val mapping = MutableMap.empty[Int, (Int, Boolean, Map[A, Int])]
        def rec(dagIndex: Int): Int = mapping.get(dagIndex).map(_._1).getOrElse {
            val dag.StateDef(accepting, fwd) = dag.indexToState(dagIndex)

            val thisForward: Map[A, Int] = fwd.map({
                case (charIndex, nextStateIndex) =>
                    val char: A = dag.indexToChar(charIndex)
                    val state = rec(nextStateIndex)
                    (char -> state)
            }).toMap

            val stateId = mapping.size

            mapping(dagIndex) = (stateId, accepting, thisForward)
            stateId
        }

        // Side-effecting!
        val initialState = rec(a.dagIndex)

        val sortedMapping = mapping.values.toList.sortBy(_._1)
        val numStates = mapping.size
        val forward = sortedMapping.map(_._3).toArray
        val finalStates = BitSet(sortedMapping.filter(_._2).map(_._1): _*)
        val definitelyMinimal = true

        new DFA[A](numStates, initialState, forward, finalStates, definitelyMinimal)
    }
}
