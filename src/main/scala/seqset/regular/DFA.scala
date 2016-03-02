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

import play.api.libs.json._

class DFA[A] protected[regular](
  private val numStates : Int,
  private val initialState : Int,
  private val forward : Array[Map[A,Int]],
  private val finalStates : BitSet,
  private val definitelyMinimal : Boolean) extends Automaton[A] {

  def stateCount : Int = numStates

  override def accepts(seq: Seq[A]) : Boolean = {
    @tailrec
    def run(s: Seq[A], c: Int) : Boolean = {
      if(s.isEmpty) {
        finalStates(c)
      } else {
        forward(c).get(s.head) match {
          case None => false
          case Some(n) => run(s.tail, n)
        }
      }
    }
    run(seq, initialState)
  }

  override def toString() : String = {
    val sb = new StringBuilder()
    sb.append(s"States  : ${stateCount}\n")
    sb.append(s"Initial : $initialState\n")
    sb.append(s"Final   : ${finalStates.mkString(", ")}\n")
    for(i <- forward.indices) {
      if(!forward(i).isEmpty) {
        sb.append(s" $i:\n")
        for((a,n) <- forward(i)) {
          sb.append(s"   $a -> $n\n")
        }
      }
    }
    sb.toString
  }

  def toJson(transitionEncoder: A=>String) : JsValue = {
    toJson(new Writes[A] {
      def writes(a: A) : JsValue = Json.parse(transitionEncoder(a))
    })
  }

  def toJson(implicit writes: Writes[A]) : JsValue = {
    val transitions = JsArray(
      for(i <- forward.indices;
          (a,n) <- forward(i)) yield {
            JsObject(Seq(
              "src"   -> JsNumber(i),
              "label" -> Json.toJson(a),
              "dest"  -> JsNumber(n)
            ))
      }
    )

    val result = JsObject(Seq(
      "states"      -> JsNumber(stateCount),
      "initial"     -> JsNumber(initialState),
      "final"       -> JsArray(finalStates.toList.map(JsNumber(_))),
      "transitions" -> transitions
    ))

    result 
  }

  def minimized : DFA[A] = if(definitelyMinimal) {
    this
  } else {
    // FIXME prune unreachable state
    
    val (fs,nfs) = (0 until numStates).partition(s => finalStates(s))

    if(fs.isEmpty) {
      return DFA.empty[A]
    }

    // state   -> classID
    val classes : Array[Int] = Array.tabulate(numStates) { s =>
      if(finalStates(s)) 0 else 1
    }

    // FIXME: this is wrong if all states are final.
    var numClasses : Int = if(nfs.isEmpty) 1 else 2
    // classID -> size
    val classSizes: Array[Int] = Array.fill(numStates)(0)
    classSizes(0) = finalStates.size
    classSizes(1) = numStates - classSizes(0)

    // Whether it created new classes.
    def splitUpClass(cid: Int) : Boolean = {
      //println("Now considering eq. class " + cid)
      if(classSizes(cid) <= 1) {
        //println("...size 1. Done.")
        false
      } else {
        val inClass = classes.indices.filter(i => classes(i) == cid)
        //println("In class " + inClass.mkString(", "))
        val outFuns = inClass.map(i => (i, forward(i)))
        val outAbs  = outFuns.map { p =>
          (p._1, p._2.mapValues(classes(_)))
        }
        // the magic.
        val partitioned : Seq[Seq[(Int,Map[A,Int])]] = outAbs.groupBy(_._2).values.toSeq
        //println("Partition has size : " + partitioned.size)
        if(partitioned.size == 1) {
          false
        } else {
          classSizes(cid) = partitioned.head.size
          //println("  in head : " + partitioned.head.map(_._1).mkString(", "))
          for(np <- partitioned.tail) {
            //println("  in tail : " + np.map(_._1).mkString(", "))
            val newClass = numClasses
            numClasses += 1
            classSizes(newClass) = np.size
            for((s, _) <- np) {
              classes(s) = newClass
            }
          }
          true
        }
      }
    }

    var C = -1
    while(numClasses > C) {
      C = numClasses

      // FIXME can we improve this by not always starting at 0?
      var c = 0
      while(c < C) {
        splitUpClass(c)
        c += 1
      }
    }

    if(numClasses == numStates) {
      // Same DFA, but we mark as definitely minimal.
      new DFA[A](numStates, initialState, forward, finalStates, true)
    } else {
      val newForward: Array[Map[A,Int]] = Array.fill(numClasses)(Map.empty)
      var newFinal  : BitSet            = BitSet.empty

      for(cid <- 0 until numClasses) {
        val firstOfClass : Int = classes.indices.find(i => classes(i) == cid).get
        newForward(cid) = forward(firstOfClass).mapValues(classes)
        if(finalStates(firstOfClass)) {
          newFinal = newFinal + cid
        }
      }

      new DFA[A](numClasses, classes(initialState), newForward, newFinal, true)
    }
  }

  private def reachable : BitSet = {
    import scala.collection.mutable.Queue

    val q = Queue.empty[Int]
    var ss = BitSet.empty

    q += initialState
    ss = ss + initialState

    while(!q.isEmpty) {
      val s = q.dequeue()
      val rs = forward(s).values
      for(r <- rs if !ss(r)) {
        q += r
        ss = ss + r
      }
    }

    ss
  }

  private def bisimulates(other: DFA[A]) : Boolean = {
    import scala.collection.mutable.{ Set => MSet }

    val m = MSet.empty[(Int,Int)]

    def bsim(a1: DFA[A], s1: Int, a2: DFA[A], s2: Int) : Boolean = if(m((s1,s2))) true else {
      if(a1.finalStates(s1) != a2.finalStates(s2)) {
        false
      } else {
        val o1 = a1.forward(s1).keySet
        val o2 = a2.forward(s2).keySet
        if(o1 != o2) {
          false
        } else {
          m += ((s1,s2))

          o1.foldLeft(true) { (b,k) =>
            b && bsim(a1, a1.forward(s1)(k), a2, a2.forward(s2)(k))
          }
        }
      }
    }

    val a1 = this.minimized
    val i1 = a1.initialState
    val a2 = other.minimized
    val i2 = a2.initialState

    if(a1.stateCount != a2.stateCount || a1.finalStates.size != a2.finalStates.size) {
      false
    } else {
      //println(s"Checking bsim relation of automata with ${a1.stateCount} state(s)...")
      //if(a1.stateCount > 71) {
      //  println("### A ###")
      //  println(a1)
      //  println("### B ###")
      //  println(a2)
      //  println("### OVER ###")
      //}
      val r = bsim(a1, i1, a2, i2)
      //println(s"...done!")
      r
    }
  }

  override lazy val toNFA: NFA[A] = {
    new NFA[A](
      numStates     = numStates,
      initialStates = BitSet(initialState),
      forward       = forward.map(_.mapValues(s => BitSet(s))),
      finalStates   = finalStates
    )
  }

  override lazy val cardinality : Cardinality = {
    Unknown
  }

  override val toDFA: DFA[A] = this

  override def tails(head: A) : DFA[A] = {
    forward(initialState).get(head) map { snd =>
      new DFA[A](
        numStates,
        snd,
        forward,
        finalStates,
        false
      )
    } getOrElse {
      DFA.empty[A]
    }
  }

  override def filterHeads(f: A=>Boolean) : DFA[A] = {
    val fstFwd: Map[A,Int] = forward(initialState)
    val filteredFstFwd: Map[A,Int] = fstFwd.toSeq.filter(p => f(p._1)).toMap
    
    val newFwd = forward.updated(initialState, filteredFstFwd)
    new DFA[A](
      numStates,
      initialState,
      newFwd,
      finalStates,
      false
    )
  }

  override def equals(other: Any) : Boolean = {
    if(other == null || !other.isInstanceOf[AnyRef])
      return false

    if(other.asInstanceOf[AnyRef] eq this)
      return true

    if(!other.isInstanceOf[Automaton[A]])
      return false

    this.bisimulates(other.asInstanceOf[Automaton[A]].toDFA)
  }

  private def rightLangIterator(state: Int) : Iterator[Seq[A]] = {
    val its = forward(state).toIterator.flatMap { case (h, n) =>
      rightLangIterator(n).map(h +: _)
    }

    if(finalStates(state)) {
      Iterator(Seq.empty[A]) ++ its
    } else {
      its
    }
  }

  override def iterator : Iterator[Seq[A]] = rightLangIterator(initialState)
}

object DFA {
  def empty[A] : DFA[A] = {
    new DFA[A](1, 0, Array(Map.empty), BitSet.empty, true)
  }

  def apply[A](seqs: Seq[A]*) : DFA[A] = {
    import seqset.trie.Trie
    // FIXME:
    // for a large number of sequences, this is not the best method
    // (keep sorting/grouping by heads instead).
    // Note that the fast method should really be in Trie.
    val trie = seqs.foldLeft[Trie[A]](Trie.empty)(_ + _)

    val stateCount = trie.nodeCount

    val forward : Array[Map[A,Int]]  = Array.fill(stateCount)(Map.empty)
    var finalStates : BitSet = BitSet.empty

    var at : Int = 0

    def rec(t: Trie[A]) : Int = {
      val here = at
      at += 1
      if(t.contains(Seq.empty)) {
        finalStates = finalStates + here
      }
      val out: Seq[A] = t.heads.toSeq
      val outStates = out.zip(out.map(h => rec(t.postfixes(h))))
      forward(here) = outStates.toMap
      here
    }

    rec(trie)

    new DFA[A](
      stateCount,
      0,
      forward,
      finalStates,
      false
    )
  }
}
