/*******************************************************************************
 * Copyright (c) 2015 IBM Corporation.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package seqset

sealed abstract class Cardinality
case class  Finite(size: BigInt) extends Cardinality
case object Infinite extends Cardinality
case object Unknown extends Cardinality
