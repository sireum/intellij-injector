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

import com.intellij.psi.{PsiParameter, PsiTypeParameter}
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.ScClassParameter
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScTypeDefinition
import org.sireum.intellij.injector.Injector._

object RecordInjector {

  object Mode extends Enumeration {
    type Type = Value
    val Object, Class, Trait, Getter = Value
  }

  val supers = Seq(recordSig)

  def inject(source: ScTypeDefinition, mode: Mode.Type): Seq[String] = {
    val name = source.getName
    val params = source.getConstructors.length match {
      case 0 => Array[PsiParameter]()
      case 1 =>
        val c = source.getConstructors.head
        c.getParameterList.getParameters
      case _ => return Seq()
    }
    val tparams = Option(source.getTypeParameterList) match {
      case Some(tpl) => tpl.getTypeParameters
      case _ => Array[PsiTypeParameter]()
    }
    val targs = for (tp <- tparams) yield tp.getName
    val typeArgs = if (targs.nonEmpty) s"[${targs.mkString(", ")}]" else ""
    val tpe = if (targs.nonEmpty) s"$name$typeArgs" else name
    val typeParams = if (targs.nonEmpty) s"[${tparams.map(_.getText).mkString(", ")}]" else ""
    var r = Vector[String]()
    mode match {
      case Mode.Object =>
        val ps = for (p <- params) yield {
          s"${p.getName}: ${p.getTypeText}"
        }

        r :+= s"def apply$typeParams(${ps.mkString(", ")}): $tpe = ???"

        r :+= s"implicit def toGetter$typeParams(o: $tpe): $name.Getter$typeArgs = ???"

      {
        var unapplyTypes = Vector[String]()
        for (p <- params) {
          if (!p.getAnnotations.exists(a => hiddenAnnotation == a.getQualifiedName)) {
            unapplyTypes :+= p.getTypeText
          }
        }
        r :+= (unapplyTypes.size match {
          case 0 => s"def unapply$typeParams(o: $tpe): $scalaPkg.Boolean = ???"
          case 1 => s"def unapply$typeParams(o: $tpe): $scalaPkg.Option[${unapplyTypes.head}] = ???"
          case _ => s"def unapply$typeParams(o: $tpe): $scalaPkg.Option[(${unapplyTypes.mkString(", ")})] = ???"
        })
      }

      case Mode.Class =>

        r :+= s"override def $$clone: $tpe = ???"

        r :+= s"override def content: $scalaPkg.Seq[($scalaPkg.String, $scalaPkg.Any)] = ???"

      {
        val ps = for (p <- params) yield s"${p.getName}: ${p.getTypeText} = ${p.getName}"
        r :+= s"def apply(${ps.mkString(", ")}): $tpe = ???"
      }

      case Mode.Trait =>

      case Mode.Getter =>
        val getters =
          for (p <- params if (p match {
            case p: ScClassParameter => !p.isVar
            case _ => true
          })) yield s"def ${p.getName}: ${p.getTypeText} = ???"
        r :+=
          s"""class Getter$typeParams(val o: $tpe) extends $scalaPkg.AnyVal {
             |  ${getters.mkString("\n  ")}
             |}""".stripMargin
    }

    mode match {
      case Mode.Class | Mode.Trait =>

        val (hasHash, hasEqual, hasString) = hasHashEqualString(source)

        if (hasHash) r :+= s"override def hashCode: $scalaPkg.Int = ???"
        else if (mode == Mode.Class) r :+= s"override def hash: $sireumPkg.Z = ???"

        if (hasEqual || mode == Mode.Class) r :+= s"override def equals(o: $scalaPkg.Any): $scalaPkg.Boolean = ???"

        if (hasString || mode == Mode.Class) r :+= s"override def toString: $javaPkg.String = ???"

        if (!hasString && mode == Mode.Class) r :+= s"override def string: $sireumString = ???"

      case _ =>
    }
    r
  }

}
