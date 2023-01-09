/*
 Copyright (c) 2017, Robby, Kansas State University
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.sireum.intellij.injector

import com.intellij.psi.PsiTypeParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTrait
import org.sireum.intellij.injector.Injector._

import scala.collection.mutable.ArrayBuffer

object SigInjector {

  val supers: Seq[String] = Seq(sig)
  val msupers: Seq[String] = Seq(msig)

  def inject(source: ScTrait): Seq[String] = {
    val name = source.getName
    val tparams = Option(source.getTypeParameterList) match {
      case Some(tpl) => tpl.getTypeParameters
      case _ => Array[PsiTypeParameter]()
    }
    val targs = for (tp <- tparams) yield tp.getName
    val typeArgs = if (targs.nonEmpty) s"[${targs.mkString(", ")}]" else ""
    val tpe = if (targs.nonEmpty) s"$name$typeArgs" else name

    var r = Vector[String]()
    val ps = getVariables(source)
    if (ps.nonEmpty) {
      r :+= s"def apply(${ps.mkString(", ")}): $tpe = ???"
    }

    val (hasHash, hasEqual, hasString) = hasHashEqualString(source)

    if (hasHash) r :+= s"override def hashCode: $scalaPkg.Int = ???"

    if (hasEqual) r :+= s"override def equals(o: $scalaPkg.Any): $scalaPkg.Boolean = ???"

    if (hasString) r :+= s"override def toString: $javaPkg.String = ???"

    if (!hasString) r :+= s"override def string: $sireumString = ???"

    r
  }

}
