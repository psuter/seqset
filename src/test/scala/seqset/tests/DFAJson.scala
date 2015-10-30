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

class DFAJson extends FlatSpec with Matchers {
  "Producing JSON output" should "behave well" in {
    val s1 : Seq[Seq[Char]] = Seq(
      "hello",
      "halo"
    )

    val a1 : DFA[Char] = DFA(s1 : _*).minimized

    val characterToJson : Char=>String = c => "\"" + c + "\""

    // println(a1.toJson(characterToJson))

    // This test should simply not throw any exception. 
  }
}
