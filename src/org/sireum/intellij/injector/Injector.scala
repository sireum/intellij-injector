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

import com.intellij.psi.PsiWhiteSpace
import com.intellij.psi.impl.source.tree.CompositeElement
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.imports.ScImportStmt
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

object Injector {
  val pkg = "org.sireum"
  val enumAnnotation = s"$pkg.enum"
  val datatypeAnnotation = s"$pkg.datatype"
  val hiddenAnnotation = s"$pkg.hidden"
  val sigAnnotation = s"$pkg.sig"
  val msigAnnotation = s"$pkg.msig"

  val sireumPkg = s"_root_.$pkg"
  val enumSig = s"$sireumPkg.EnumSig"
  val datatypeSig = s"$sireumPkg.DatatypeSig"
  val immutable = s"$sireumPkg.Immutable"
  val mutable = s"$sireumPkg.Mutable"
  val scalaPkg = "_root_.scala"
  val javaPkg = "_root_.java"

  val sireumString = s"$sireumPkg.String"

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

}

import Injector._

class Injector extends SyntheticMembersInjector {

  override def injectSupers(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return Seq()
    source match {
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `enumAnnotation` => return EnumInjector.supers
            case _ =>
          }
        }
      case source: ScTrait =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return DatatypeInjector.supers
            case `sigAnnotation` => return SigInjector.supers
            case `msigAnnotation` => return SigInjector.msupers
            case _ =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return DatatypeInjector.supers
            case _ =>
          }
        }
      case _ =>
    }
    Seq()
  }

  override def needsCompanionObject(source: ScTypeDefinition): Boolean = {
    if (!isSireum(source)) return false
    for (a <- source.getAnnotations) {
      a.getQualifiedName match {
        case `datatypeAnnotation` => return true
        case _ =>
      }
    }
    false
  }

  override def injectInners(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return Seq()
    source match {
      case source: ScTrait =>
      case source: ScClass =>
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
                case _ =>
              }
            }
          case _ =>
        }
    }
    Seq()
  }

  override def injectFunctions(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return Seq()
    source match {
      case source: ScTrait =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` =>
              return DatatypeInjector.inject(source, DatatypeInjector.Mode.Trait)
            case _ =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` =>
              return DatatypeInjector.inject(source, DatatypeInjector.Mode.Class)
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
                case _ =>
              }
            }
          case _ =>
        }
      case _ =>
    }
    Seq()
  }

  override def injectMembers(source: ScTypeDefinition): Seq[String] = {
    if (!isSireum(source)) return Seq()
    source match {
      case source: ScTrait =>
      case source: ScClass =>
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `enumAnnotation` =>
              return EnumInjector.inject(source, EnumInjector.Mode.Members)
            case _ =>
          }
        }
    }
    Seq()
  }
}
