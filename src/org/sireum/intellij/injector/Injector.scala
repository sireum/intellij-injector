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

import com.intellij.psi.{PsiAnnotation, PsiElement}
import org.jetbrains.plugins.scala.lang.psi.api.base.ScPrimaryConstructor
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.statements.params.{
  ScClassParameter,
  ScParameterType
}
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{
  ScClass,
  ScObject,
  ScTrait,
  ScTypeDefinition
}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

import scala.util.{Success, Try}

object Injector {

  final case class Parameter(name: String,
                             tpe: String,
                             annotations: Seq[PsiAnnotation],
                             isVar: Boolean,
                             isVal: Boolean)

  val pkg = "org.sireum"

  val sireumPkg = s"_root_.$pkg"
  val enumSig = s"$sireumPkg.EnumSig"
  val datatypeSig = s"$sireumPkg.DatatypeSig"
  val recordSig = s"$sireumPkg.RecordSig"
  val immutable = s"$sireumPkg.Immutable"
  val mutable = s"$sireumPkg.Mutable"
  val sig = s"$sireumPkg.SigTrait"
  val msig = s"$sireumPkg.MSigTrait"
  val scalaPkg = "_root_.scala"
  val javaPkg = "_root_.java"

  val sireumString = s"$sireumPkg.String"
  val emptyResult: Seq[String] = Seq()

  var pureSlangMode = true

  def extractBoolean(expression: ScExpression): Option[Boolean] = {
    expression.getText match {
      case "T" | "true"  => Some(true)
      case "F" | "false" => Some(false)
      case _             => None
    }
  }

  def extractInt(e: ScExpression): Option[BigInt] = {
    val text = e.getText
    if (text.startsWith("z\"\"\"") && text.endsWith("\"\"\"")) {
      Try(BigInt(normNum(text.substring(4, text.length - 3)))) match {
        case Success(n) => return Some(n)
        case _          =>
      }

    } else if (text.startsWith("z\"") && text.endsWith("\"")) {
      Try(BigInt(normNum(text.substring(2, text.length - 1)))) match {
        case Success(n) => return Some(n)
        case _          =>
      }
    } else if (text.last.toUpper == 'L') {
      Try(BigInt(text.substring(0, text.length - 1))) match {
        case Success(n) => return Some(n)
        case _          =>
      }
    } else {
      Try(BigInt(text)) match {
        case Success(n) => return Some(n)
        case _          =>
      }
    }
    None
  }

  def normNum(s: String): String = {
    val sb = new java.lang.StringBuilder(s.length)
    for (c <- s) c match {
      case ',' | ' ' | '_' =>
      case _               => sb.append(c)
    }
    sb.toString
  }

  def hasHashEqualString(
      source: ScTypeDefinition): (Boolean, Boolean, Boolean) = {
    var hasHash = false
    var hasEqual = false
    var hasString = false

    for (m <- source.extendsBlock.members) {
      m.getName match {
        case "hash"    => hasHash = true
        case "isEqual" => hasEqual = true
        case "string"  => hasString = true
        case _         =>
      }
    }
    (hasHash, hasEqual, hasString)
  }

  def zCompanionName(name: String): String = s"$$${name}Companion"

  def scName(name: String): String = name + "$Slang"

  def scPrefix(name: String): String = name.head.toLower + name.tail

  def isSireum(source: ScTypeDefinition): Boolean = {
    def detect(index: Int): Boolean = {
      val input =
        source.getContainingFile.getViewProvider.getDocument.getCharsSequence
      var i = index
      val sb = new java.lang.StringBuilder
      var c = input.charAt(i)
      while (i < input.length() && c != '\n') {
        if (!c.isWhitespace) sb.append(c)
        i = i + 1
        c = input.charAt(i)
      }
      sb.toString.contains("#Sireum")
    }
    try {
      val ext = source.getContainingFile.getVirtualFile.getExtension
      ext match {
        case "scala" | "sc" | "cmd" =>
          val input =
            source.getContainingFile.getViewProvider.getDocument.getText
          if (input.startsWith("::#!")) {
            var i = input.indexOf("::!#")
            if (i < 0) {
              return false
            }
            while (i < input.length && input(i) != '\n') {
              i = i + 1
            }
            return detect(i + 1)
          } else if (input.startsWith("::/*#!")) {
            var i = input.indexOf("::!#*/")
            if (i < 0) {
              return false
            }
            while (i < input.length && input(i) != '\n') {
              i = i + 1
            }
            return detect(i + 1)
          } else {
            return detect(0)
          }
        case _ =>
      }
      false
    } catch {
      case _: Throwable => false
    }
  }

  implicit class ScClassParameters(val t: ScTypeDefinition) extends AnyVal {
    def parameters: Seq[Parameter] = {
      def findClassPrimaryConstructor(
          e: PsiElement): Option[ScPrimaryConstructor] = {
        e match {
          case e: ScPrimaryConstructor => return Some(e)
          case _ =>
            for (c <- e.getChildren) {
              findClassPrimaryConstructor(c) match {
                case r @ Some(_) => return r
                case _           =>
              }
            }
        }
        None
      }

      def findClassParameter(e: PsiElement): Seq[ScClassParameter] = {
        e match {
          case e: ScClassParameter => Vector(e)
          case _                   => e.getChildren.flatMap(findClassParameter)
        }
      }

      def findType(p: ScClassParameter): String = {
        val typeText: String = p.getRealParameterType match {
          case Left(_) => p.expectedParamType match {
            case Some(t) => t.toPsiType.getCanonicalText
            case _ => "java.lang.Object"
          }
          case Right(value) => value.canonicalText
        }
        typeText match {
          case "java.lang.Object" =>
          case _ =>
            val st = new java.util.StringTokenizer(typeText.replace("=>", "⇒"), "⇒<>, \t\r\n", true)
            val sb = new java.lang.StringBuilder
            while (st.hasMoreTokens) sb.append(st.nextToken match {
              case "boolean"          => "_root_.org.sireum.B"
              case "char"             => "_root_.org.sireum.C"
              case "float"            => "_root_.org.sireum.F32"
              case "double"           => "_root_.org.sireum.F64"
              case "java.lang.String" => "_root_.org.sireum.String"
              case "spire.math.Real"  => "_root_.org.sireum.R"
              case "<"                => "["
              case ">"                => "]"
              case "⇒"                => " => "
              case s                  => s.trim
            })
            return sb.toString
        }
        for (c <- p.getChildren) c match {
          case c: ScParameterType => return c.getText
          case _                  =>
        }
        "scala.Any"
      }

      findClassPrimaryConstructor(t) match {
        case Some(c) =>
          for (p <- findClassParameter(c))
            yield
              Parameter(p.getName,
                        findType(p),
                        p.getAnnotations,
                        p.isVar,
                        p.isVal)
        case _ => Seq()
      }
    }
  }

  def getAnnotationName(a: PsiAnnotation): String = {
    var s = a.getText
    val i = s.indexOf('(')
    s = if (i >= 0) s.substring(1, i) else s.substring(1)
    s.trim
  }

}

import Injector._

class Injector extends SyntheticMembersInjector {

  override def injectSupers(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return emptyResult
    source match {
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "enum" => return EnumInjector.supers
            case _      =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              getAnnotationName(a) match {
                case "range" =>
                  return RangeInjector.objectSupers(c)
                case "bits" =>
                  return BitsInjector.objectSupers(c)
                case _ =>
              }
            }
          case _ =>
        }
      case source: ScTrait =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "datatype" => return DatatypeInjector.supers
            case "record"   => return RecordInjector.supers
            case "sig"      => return SigInjector.supers
            case "msig"     => return SigInjector.msupers
            case _          =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "datatype" => return DatatypeInjector.supers
            case "record"   => return RecordInjector.supers
            case "range"    => return RangeInjector.supers(source)
            case "bits" =>
              return BitsInjector.inject(source, a, BitsInjector.Mode.Supers)
            case _ =>
          }
        }
      case _ =>
    }
    emptyResult
  }

  override def needsCompanionObject(source: ScTypeDefinition): Boolean = {
    if (!isSireum(source)) return false
    for (a <- source.getAnnotations) {
      getAnnotationName(a) match {
        case "datatype" => return true
        case "record"   => return true
        case "range"    => return true
        case "bits"     => return true
        case _          =>
      }
    }
    false
  }

  override def injectInners(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return emptyResult
    source match {
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "enum" =>
              return EnumInjector.inject(source, EnumInjector.Mode.Inners)
            case _ =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              getAnnotationName(a) match {
                case "datatype" =>
                  return DatatypeInjector.inject(c,
                                                 DatatypeInjector.Mode.Getter)
                case "record" =>
                  return RecordInjector.inject(c, RecordInjector.Mode.Getter)
                case "range" =>
                  return RangeInjector.inject(c,
                                              a,
                                              RangeInjector.Mode.ObjectInners)
                case "bits" =>
                  return BitsInjector.inject(c,
                                             a,
                                             BitsInjector.Mode.ObjectInners)
                case _ =>
              }
            }
          case _ =>
        }
      case _ =>
    }
    emptyResult
  }

  override def injectFunctions(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return emptyResult
    source match {
      case source: ScTrait =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "datatype" =>
              return DatatypeInjector.inject(source,
                                             DatatypeInjector.Mode.Trait)
            case "record" =>
              return RecordInjector.inject(source, RecordInjector.Mode.Trait)
            case _ =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "datatype" =>
              return DatatypeInjector.inject(source,
                                             DatatypeInjector.Mode.Class)
            case "record" =>
              return RecordInjector.inject(source, RecordInjector.Mode.Class)
            case "range" =>
              return RangeInjector.inject(source, a, RangeInjector.Mode.Class)
            case "bits" =>
              return BitsInjector.inject(source, a, BitsInjector.Mode.Class)
            case _ =>
          }
        }
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "enum" =>
              return EnumInjector.inject(source, EnumInjector.Mode.Functions)
            case _ =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              getAnnotationName(a) match {
                case "datatype" =>
                  return DatatypeInjector.inject(c,
                                                 DatatypeInjector.Mode.Object)
                case "record" =>
                  return RecordInjector.inject(c, RecordInjector.Mode.Object)
                case "range" =>
                  return RangeInjector.inject(c, a, RangeInjector.Mode.Object)
                case "bits" =>
                  return BitsInjector.inject(c, a, BitsInjector.Mode.Object)
                case _ =>
              }
            }
          case _ =>
        }
      case _ =>
    }
    emptyResult
  }

  override def injectMembers(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return emptyResult
    source match {
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          getAnnotationName(a) match {
            case "enum" =>
              return EnumInjector.inject(source, EnumInjector.Mode.Members)
            case _ =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              getAnnotationName(a) match {
                case "range" =>
                  return RangeInjector.inject(c,
                                              a,
                                              RangeInjector.Mode.ObjectMembers)
                case "bits" =>
                  return BitsInjector.inject(c,
                                             a,
                                             BitsInjector.Mode.ObjectMembers)
                case _ =>
              }
            }
          case _ =>
        }
      case _ =>
    }
    emptyResult
  }
}
