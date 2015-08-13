/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package seqset
package regular

import scala.annotation.tailrec

import scala.collection.immutable.BitSet

class NFA[A] protected[regular](
  private val numStates : Int,
  private val initialStates : BitSet,
  private val forward : Array[Map[A,BitSet]],
  private val finalStates : BitSet) extends Automaton[A] {

  def stateCount : Int = numStates

  private def follow(states: BitSet, symbol: A) : BitSet = {
    states.foldLeft(BitSet.empty) { (set, state) =>
      forward(state).get(symbol).map { ns =>
        set ++ ns 
      } getOrElse {
        set
      }
    }
  }

  private def includesFinal(states: BitSet) : Boolean = !(states.intersect(finalStates).isEmpty)

  override def accepts(seq: Seq[A]) : Boolean = {
    @tailrec
    def run(s: Seq[A], cs: BitSet) : Boolean = {
      if(s.isEmpty) {
        includesFinal(cs)
      } else {
        val h = s.head
        val ns = follow(cs, h)
        if(ns.isEmpty) {
          false
        } else {
          run(s.tail, ns)
        }
      }
    }

    run(seq, initialStates)
  }

  override def toString() : String = {
    val sb = new StringBuilder()
    sb.append(s"States  : ${stateCount}\n")
    sb.append(s"Initial : ${initialStates.mkString(", ")}\n")
    sb.append(s"Final   : ${finalStates.mkString(", ")}\n")
    for(i <- forward.indices) {
      if(!forward(i).isEmpty) {
        sb.append(s" $i:\n")
        for((a,ns) <- forward(i)) {
          sb.append(s"   $a -> ${ns.mkString(", ")}\n")
        }
      }
    }
    sb.toString
  }

  private def determinized : DFA[A] = {
    import scala.collection.mutable.{ Buffer, Set=>MSet, Map=>MMap }
    import scala.collection.mutable.Queue

    val newStates  = MMap.empty[BitSet,Int]
    val newForward = Buffer.empty[Map[A,Int]]
    val mapping    = Buffer.empty[BitSet]

    val q = Queue.empty[(Int,BitSet)]

    def getOrCreate(ss: BitSet) : Int = newStates.getOrElse(ss, {
      val n = newStates.size

      newStates(ss)  = n
      newForward.append(Map.empty)
      mapping.append(ss)

      q += ((n, ss))

      n
    })

    def allOutEdges(ss: BitSet) : Set[A] = ss.flatMap(s => forward(s).keys)

    val initialState = getOrCreate(initialStates)

    while(!q.isEmpty) {
      val (s, ss) = q.dequeue()
      val fm = allOutEdges(ss).toSeq.map(a => a -> getOrCreate(follow(ss, a)))
      newForward(s) = fm.toMap
    }

    val newFinal = mapping.map(includesFinal)

    new DFA[A](
      mapping.length,
      initialState,
      newForward.toArray,
      (0 until mapping.length).foldLeft[BitSet](BitSet.empty) { (set,i) =>
        if(newFinal(i)) {
          set + i
        } else {
          set
        }
      },
      false
    )
  }

  def union(other: NFA[A]) : NFA[A] = if(other eq this) {
    this
  } else {
    val nofw = other.forward.toSeq.map(fm => fm.mapValues(_.map(_ + numStates)))
    val newForward : Array[Map[A,BitSet]] = Array((forward.toSeq ++ nofw) : _*)
    val newFinal   : BitSet               = finalStates ++ other.finalStates.map(_ + numStates)

    new NFA[A](
      numStates + other.numStates,
      initialStates ++ other.initialStates.map(_ + numStates),
      newForward,
      newFinal
    )
  }

  def concat(other: NFA[A]) : NFA[A] = {
    val nofw = other.forward.toSeq.map(fm => fm.mapValues(_.map(_ + numStates)))

    val rightInitial : BitSet = other.initialStates.map(_ + numStates)

    val ntfw = forward.toSeq.map { fm =>
      fm.mapValues { ts =>
        if(includesFinal(ts)) {
          ts ++ rightInitial
        } else {
          ts
        }
      }
    }

    val newForward = Array((ntfw ++ nofw) : _*)

    val newFinal = other.finalStates.map(_ + numStates)

    new NFA[A](
      numStates + other.numStates,
      initialStates,
      newForward,
      newFinal
    )
  }

  override lazy val toDFA: DFA[A] = determinized

  override val toNFA: NFA[A] = this

  override def equals(other: Any) : Boolean = {
    if(other == null || !other.isInstanceOf[AnyRef])
      return false

    if(other.asInstanceOf[AnyRef] eq this)
      return true

    if(!other.isInstanceOf[Automaton[A]])
      return false

    this.toDFA.equals(other.asInstanceOf[Automaton[A]].toDFA)
  } 
}
