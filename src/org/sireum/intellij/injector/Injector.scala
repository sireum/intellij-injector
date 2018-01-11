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

import com.intellij.psi.{PsiType, PsiWhiteSpace}
import com.intellij.psi.impl.source.tree.CompositeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScExpression
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportStmt
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

import scala.util.{Success, Try}

object Injector {
  val pkg = "org.sireum"
  val enumAnnotation = s"$pkg.enum"
  val datatypeAnnotation = s"$pkg.datatype"
  val recordAnnotation = s"$pkg.record"
  val hiddenAnnotation = s"$pkg.hidden"
  val sigAnnotation = s"$pkg.sig"
  val msigAnnotation = s"$pkg.msig"
  val bitsAnnotation = s"$pkg.bits"
  val rangeAnnotation = s"$pkg.range"

  val sireumPkg = s"_root_.$pkg"
  val enumSig = s"$sireumPkg.EnumSig"
  val datatypeSig = s"$sireumPkg.DatatypeSig"
  val recordSig = s"$sireumPkg.RecordSig"
  val immutable = s"$sireumPkg.Immutable"
  val mutable = s"$sireumPkg.Mutable"
  val scalaPkg = "_root_.scala"
  val javaPkg = "_root_.java"

  val sireumString = s"$sireumPkg.String"
  val emptyResult: Seq[String] = Seq()

  def extractBoolean(expression: ScExpression): Option[Boolean] = {
    expression.getText match {
      case "T" | "true" => Some(true)
      case "F" | "false" => Some(false)
      case _ => None
    }
  }

  def extractInt(e: ScExpression): Option[BigInt] = {
    val text = e.getText
    if (text.startsWith("z\"\"\"") && text.endsWith("\"\"\"")) {
      Try(BigInt(normNum(text.substring(4, text.length - 3)))) match {
        case Success(n) => return Some(n)
        case _ =>
      }

    } else if (text.startsWith("z\"") && text.endsWith("\"")) {
      Try(BigInt(normNum(text.substring(2, text.length - 1)))) match {
        case Success(n) => return Some(n)
        case _ =>
      }
    } else if (text.last.toUpper == 'L') {
      Try(BigInt(text.substring(0, text.length - 1))) match {
        case Success(n) => return Some(n)
        case _ =>
      }
    } else {
      Try(BigInt(text)) match {
        case Success(n) => return Some(n)
        case _ =>
      }
    }
    None
  }

  def normNum(s: String): String = {
    val sb = new java.lang.StringBuilder(s.length)
    for (c <- s) c match {
      case ',' | ' ' | '_' =>
      case _ => sb.append(c)
    }
    sb.toString
  }

  def hasHashEqualString(source: ScTypeDefinition): (Boolean, Boolean, Boolean) = {
    var hasHash = false
    var hasEqual = false
    var hasString = false

    for (m <- source.extendsBlock.members) {
      m.getName match {
        case "hash" => hasHash = true
        case "isEqual" => hasEqual = true
        case "string" => hasString = true
        case _ =>
      }
    }
    (hasHash, hasEqual, hasString)
  }

  def zCompanionName(name: String): String = s"$$${name}Companion"

  def iSName(name: String): String = "IS" + name

  def mSName(name: String): String = "MS" + name

  def scName(name: String): String = name + "$Slang"

  def scPrefix(name: String): String = name.head.toLower + name.tail

  def isSireum(source: ScTypeDefinition): Boolean = try {
    val ext = source.getContainingFile.getVirtualFile.getExtension
    ext match {
      case "scala" =>
        val input = source.getContainingFile.getViewProvider.getDocument.getCharsSequence
        var i = 0
        val sb = new java.lang.StringBuilder
        var c = input.charAt(i)
        while (i < input.length() && c != '\n') {
          if (!c.isWhitespace) sb.append(c)
          i = i + 1
          c = input.charAt(i)
        }
        return sb.toString.contains("#Sireum")
      case "sc" =>
        val node = source.getContainingFile.getNode
        val children = node.getChildren(null)
        var i = 0
        while (children(i).isInstanceOf[PsiWhiteSpace]) i += 1
        children(i) match {
          case e: CompositeElement => e.getPsi match {
            case stmt: ScImportStmt =>
              val exprs = stmt.importExprs
              if (exprs.length != 1) return false
              if (!exprs.head.isSingleWildcard) return false
              exprs.head.qualifier.qualName match {
                case "org.sireum" | "org.sireum.logika" => return true
                case _ =>
              }
            case _ =>
          }
          case _ =>
        }
      case _ =>
    }
    false
  } catch {
    case _: Throwable => false
  }

  implicit class TypeString(val t: PsiType) extends AnyVal {
    def getText: String =
      t.getCanonicalText.replaceAllLiterally("<", "[").replaceAllLiterally(">", "]")
  }

}

import Injector._

class Injector extends SyntheticMembersInjector {

  override def injectSupers(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return emptyResult
    source match {
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `enumAnnotation` => return EnumInjector.supers
            case _ =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              a.getQualifiedName match {
                case `rangeAnnotation` =>
                  return RangeInjector.objectSupers(c)
                case `bitsAnnotation` =>
                  return BitsInjector.objectSupers(c)
                case _ =>
              }
            }
          case _ =>
        }
      case source: ScTrait =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return DatatypeInjector.supers
            case `recordAnnotation` => return RecordInjector.supers
            case `sigAnnotation` => return SigInjector.supers
            case `msigAnnotation` => return SigInjector.msupers
            case _ =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return DatatypeInjector.supers
            case `recordAnnotation` => return RecordInjector.supers
            case `rangeAnnotation` => return RangeInjector.supers(source)
            case `bitsAnnotation` => return BitsInjector.inject(source, a, BitsInjector.Mode.Supers)
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
      a.getQualifiedName match {
        case `datatypeAnnotation` => return true
        case `recordAnnotation` => return true
        case `rangeAnnotation` => return true
        case `bitsAnnotation` => return true
        case _ =>
      }
    }
    false
  }

  override def injectInners(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return emptyResult
    source match {
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `enumAnnotation` =>
              return EnumInjector.inject(source, EnumInjector.Mode.Inners)
            case _ =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              a.getQualifiedName match {
                case `datatypeAnnotation` =>
                  return DatatypeInjector.inject(c, DatatypeInjector.Mode.Getter)
                case `recordAnnotation` =>
                  return RecordInjector.inject(c, RecordInjector.Mode.Getter)
                case `rangeAnnotation` =>
                  return RangeInjector.inject(c, a, RangeInjector.Mode.ObjectInners)
                case `bitsAnnotation` =>
                  return BitsInjector.inject(c, a, BitsInjector.Mode.ObjectInners)
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
          a.getQualifiedName match {
            case `datatypeAnnotation` =>
              return DatatypeInjector.inject(source, DatatypeInjector.Mode.Trait)
            case `recordAnnotation` =>
              return RecordInjector.inject(source, RecordInjector.Mode.Trait)
            case _ =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` =>
              return DatatypeInjector.inject(source, DatatypeInjector.Mode.Class)
            case `recordAnnotation` =>
              return RecordInjector.inject(source, RecordInjector.Mode.Class)
            case `rangeAnnotation` =>
              return RangeInjector.inject(source, a, RangeInjector.Mode.Class)
            case `bitsAnnotation` =>
              return BitsInjector.inject(source, a, BitsInjector.Mode.Class)
            case _ =>
          }
        }
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `enumAnnotation` =>
              return EnumInjector.inject(source, EnumInjector.Mode.Functions)
            case _ =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              a.getQualifiedName match {
                case `datatypeAnnotation` =>
                  return DatatypeInjector.inject(c, DatatypeInjector.Mode.Object)
                case `recordAnnotation` =>
                  return RecordInjector.inject(c, RecordInjector.Mode.Object)
                case `rangeAnnotation` =>
                  return RangeInjector.inject(c, a, RangeInjector.Mode.Object)
                case `bitsAnnotation` =>
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
          a.getQualifiedName match {
            case `enumAnnotation` =>
              return EnumInjector.inject(source, EnumInjector.Mode.Members)
            case _ =>
          }
        }
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              a.getQualifiedName match {
                case `rangeAnnotation` =>
                  return RangeInjector.inject(c, a, RangeInjector.Mode.ObjectMembers)
                case `bitsAnnotation` =>
                  return BitsInjector.inject(c, a, BitsInjector.Mode.ObjectMembers)
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
