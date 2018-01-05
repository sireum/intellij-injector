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

package org.sireum.intellij

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScObject, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.SyntheticMembersInjector

object Injector {
  val pkg = "org.sireum"
  val enumAnnotation = s"$pkg.enum"
  val datatypeAnnotation = s"$pkg.datatype"
  val hiddenAnnotation = s"$pkg.hidden"

  val sireumPkg = s"_root_.$pkg"
  val enumSig = s"$sireumPkg.EnumSig"
  val datatypeSig = s"$sireumPkg.DatatypeSig"
  val scalaPkg = "_root.scala"
  val javaPkg = "_root.java"

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

}

import Injector._

class Injector extends SyntheticMembersInjector {
  override def injectSupers(source: ScTypeDefinition): Seq[String] = {
    source match {
      case source: ScObject =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `enumAnnotation` => return Seq(enumSig)
            case _ =>
          }
        }
      case source: ScTrait =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return Seq(datatypeSig)
            case _ =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return Seq(datatypeSig)
            case _ =>
          }
        }
      case _ =>
    }
    Seq()
  }

  override def needsCompanionObject(source: ScTypeDefinition): Boolean = {
    for (a <- source.getAnnotations) {
      a.getQualifiedName match {
        case `datatypeAnnotation` => return true
        case _ =>
      }
    }
    false
  }

  override def injectInners(source: ScTypeDefinition): Seq[String] = {
    source match {
      case source: ScTrait =>
      case source: ScClass =>
      case source: ScObject =>
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              a.getQualifiedName match {
                case `datatypeAnnotation` => return DatatypeInjector.inject(c, DatatypeInjector.Mode.Getter)
                case _ =>
              }
            }
          case _ =>
        }
    }
    Seq()
  }

  override def injectFunctions(source: ScTypeDefinition): Seq[String] = {
    source match {
      case source: ScTrait =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return DatatypeInjector.inject(source, DatatypeInjector.Mode.Trait)
            case _ =>
          }
        }
      case source: ScClass =>
        for (a <- source.getAnnotations) {
          a.getQualifiedName match {
            case `datatypeAnnotation` => return DatatypeInjector.inject(source, DatatypeInjector.Mode.Class)
            case _ =>
          }
        }
      case source: ScObject =>
        source.fakeCompanionClassOrCompanionClass match {
          case c: ScClass =>
            for (a <- c.getAnnotations) {
              a.getQualifiedName match {
                case `datatypeAnnotation` => return DatatypeInjector.inject(c, DatatypeInjector.Mode.Object)
                case _ =>
              }
            }
          case _ =>
        }
      case _ =>
    }
    Seq()
  }
}
