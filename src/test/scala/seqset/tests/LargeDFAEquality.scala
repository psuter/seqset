/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0,
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package seqset
package regular
package tests

import org.scalatest._

import scala.collection.immutable.BitSet

class LargeDFAEquality extends FlatSpec with Matchers {
  val at = Array.fill(72)(Map.empty[Int,Int])

  // initial: 50, final: 0,
  at(1) = Map(94 -> 54)
  at(2) = Map(116 -> 56, 89 -> 1, 104 -> 65, 53 -> 10, 38 -> 40, 74 -> 28)
  at(3) = Map(276 -> 44)
  at(4) = Map(185 -> 29)
  at(5) = Map(219 -> 58, 169 -> 64, 329 -> 68, 389 -> 51, 427 -> 59, 150 -> 39, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 138 -> 37, 204 -> 21, 231 -> 9, 195 -> 60, 268 -> 7, 374 -> 12, 344 -> 41, 182 -> 4, 299 -> 42, 402 -> 30, 359 -> 48)
  at(6) = Map(156 -> 34)
  at(7) = Map(271 -> 69)
  at(8) = Map(329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 314 -> 13, 374 -> 12, 344 -> 41, 402 -> 30, 359 -> 48)
  at(9) = Map(234 -> 70, 259 -> 14)
  at(10) = Map(58 -> 53)
  at(11) = Map(427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 402 -> 30)
  at(12) = Map(379 -> 62)
  at(13) = Map(319 -> 22)
  at(14) = Map(237 -> 16)
  at(15) = Map(329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 314 -> 13, 374 -> 12, 344 -> 41, 299 -> 42, 402 -> 30, 359 -> 48)
  at(16) = Map(264 -> 44)
  at(17) = Map(389 -> 51, 427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 374 -> 12, 402 -> 30, 359 -> 48)
  at(18) = Map(240 -> 71)
  at(19) = Map(219 -> 58, 329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 204 -> 21, 231 -> 9, 195 -> 60, 268 -> 7, 374 -> 12, 344 -> 41, 182 -> 4, 299 -> 42, 402 -> 30, 359 -> 48)
  at(20) = Map(116 -> 56, 89 -> 1, 104 -> 65, 53 -> 10, 74 -> 28)
  at(21) = Map(209 -> 55)
  at(22) = Map(329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 374 -> 12, 344 -> 41, 402 -> 30, 359 -> 48)
  at(23) = Map(5 -> 47)
  at(24) = Map(469 -> 0)
  at(25) = Map(89 -> 1, 104 -> 65, 116 -> 56)
  at(26) = Map(74 -> 28, 89 -> 1, 104 -> 65, 116 -> 56)
  at(27) = Map(329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 231 -> 9, 268 -> 7, 374 -> 12, 344 -> 41, 299 -> 42, 402 -> 30, 359 -> 48)
  at(28) = Map(79 -> 25)
  at(29) = Map(219 -> 58, 329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 204 -> 21, 231 -> 9, 195 -> 60, 268 -> 7, 374 -> 12, 344 -> 41, 299 -> 42, 402 -> 30, 359 -> 48)
  at(30) = Map(405 -> 32)
  at(31) = Map(454 -> 49)
  at(32) = Map(414 -> 43, 427 -> 59, 442 -> 45, 454 -> 49)
  at(33) = Map(389 -> 51, 427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 374 -> 12, 402 -> 30)
  at(34) = Map(159 -> 52)
  at(35) = Map(427 -> 59, 442 -> 45, 454 -> 49)
  at(36) = Map(259 -> 14)
  at(37) = Map(219 -> 58, 169 -> 64, 329 -> 68, 389 -> 51, 427 -> 59, 150 -> 39, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 204 -> 21, 231 -> 9, 195 -> 60, 268 -> 7, 374 -> 12, 344 -> 41, 182 -> 4, 299 -> 42, 402 -> 30, 359 -> 48)
  at(38) = Map(29 -> 2)
  at(39) = Map(153 -> 6)
  at(40) = Map(43 -> 20)
  at(41) = Map(349 -> 17)
  at(42) = Map(304 -> 8)
  at(43) = Map(417 -> 35)
  at(44) = Map(329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 374 -> 12, 344 -> 41, 299 -> 42, 402 -> 30, 359 -> 48)
  at(45) = Map(447 -> 31)
  at(46) = Map(460 -> 24)
  at(47) = Map(15 -> 61, 10 -> 61)
  at(48) = Map(364 -> 33)
  at(49) = Map(457 -> 46)
  at(50) = Map(3 -> 23, 99999 -> 0)
  at(51) = Map(392 -> 11)
  at(52) = Map(219 -> 58, 169 -> 64, 329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 204 -> 21, 231 -> 9, 195 -> 60, 268 -> 7, 374 -> 12, 344 -> 41, 182 -> 4, 299 -> 42, 402 -> 30, 359 -> 48)
  at(53) = Map(64 -> 26)
  at(54) = Map(104 -> 65, 116 -> 56)
  at(55) = Map(219 -> 58, 329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 231 -> 9, 268 -> 7, 374 -> 12, 344 -> 41, 299 -> 42, 402 -> 30, 359 -> 48)
  at(56) = Map(131 -> 5, 124 -> 5)
  at(57) = Map(116 -> 56)
  at(58) = Map(222 -> 27)
  at(59) = Map(432 -> 66)
  at(60) = Map(219 -> 58, 329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 204 -> 21, 231 -> 9, 268 -> 7, 374 -> 12, 344 -> 41, 299 -> 42, 402 -> 30, 359 -> 48)
  at(61) = Map(116 -> 56, 24 -> 38, 89 -> 1, 104 -> 65, 53 -> 10, 38 -> 40, 74 -> 28)
  at(62) = Map(389 -> 51, 427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 402 -> 30)
  at(63) = Map(389 -> 51, 427 -> 59, 454 -> 49, 442 -> 45, 414 -> 43, 374 -> 12, 344 -> 41, 402 -> 30, 359 -> 48)
  at(64) = Map(172 -> 19)
  at(65) = Map(109 -> 57)
  at(66) = Map(442 -> 45, 454 -> 49)
  at(67) = Map(289 -> 15)
  at(68) = Map(334 -> 63)
  at(69) = Map(237 -> 3)
  at(70) = Map(237 -> 18)
  at(71) = Map(329 -> 68, 389 -> 51, 427 -> 59, 454 -> 49, 286 -> 67, 442 -> 45, 414 -> 43, 314 -> 13, 231 -> 36, 268 -> 7, 374 -> 12, 344 -> 41, 299 -> 42, 402 -> 30, 359 -> 48)

  val a1 = new DFA[Int](72, 50, at, BitSet(0), false)
  val a2 = new DFA[Int](72, 50, at, BitSet(0), false)

  "A large automaton" should "be equal to itself" in {
    a1.equals(a2) should be (true)
  }
}
