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

import org.jetbrains.plugins.scala.lang.psi.api.base.ScLiteral
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScObject
import org.sireum.intellij.injector.Injector._

object EnumInjector {

  object Mode extends Enumeration {
    type Type = Value
    val Functions, Inners, Members = Value
  }

  val supers: Seq[String] = Seq(enumSig)

  def inject(source: ScObject, mode: Mode.Type): Seq[String] = {

    var r = Vector[String]()

    mode match {

      case Mode.Functions =>

        r :+= s"final def byName(name: $sireumString): $sireumPkg.Option[Type] = ???"

        r :+= s"final def byOrdinal(ordinal: $sireumPkg.Z): $sireumPkg.Option[Type] = ???"

      case Mode.Inners =>
        r :+=
          s"""sealed trait Type extends $scalaPkg.Ordered[Type] {
             |  def ordinal: $sireumPkg.Z
             |  def name: $sireumString
             |  final def hash: $sireumPkg.Z = ???
             |  final def isEqual(other: Type): $sireumPkg.B = ???
             |  final def compare(that: Type): $scalaPkg.Int = ???
             |}
         """.stripMargin

        for (b <- source.extendsBlock.templateBody;
             e <- b.exprs) {
          e match {
            case e: ScLiteral if e.isSymbol =>
              val name = e.getValue.toString.substring(1)
              r :+=
                s"""final case object $name extends Type {
                   |  def ordinal: $sireumPkg.Z = ???
                   |  def name: $sireumString = ???
                   |}
                 """.stripMargin
            case _ =>
          }
        }

      case Mode.Members =>

        r :+= s"val numOfElements: $sireumPkg.Z = ???"

        r :+= s"val elements: $sireumPkg.ISZ[Type] = ???"

    }

    r
  }

}
