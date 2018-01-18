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

import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.ScClass
import Injector._
import com.intellij.psi.PsiAnnotation
import org.jetbrains.plugins.scala.lang.psi.api.expr.{ScAnnotation, ScAssignStmt}

object RangeInjector {

  object Mode extends Enumeration {
    type Type = Value
    val Object, Class, ObjectInners, ObjectMembers = Value
  }

  def supers(source: ScClass): Seq[String] =
    Seq(s"$sireumPkg.Z.Range[${source.getName}]")

  def objectSupers(source: ScClass): Seq[String] =
    Seq(s"$sireumPkg.$$ZCompanion[${source.getName}]")

  def inject(source: ScClass, annotation: PsiAnnotation, mode: Mode.Type): Seq[String] = {
    val args = annotation match {
      case annotation: ScAnnotation =>
        val argss = annotation.constructor.arguments
        if (argss.size != 1) return emptyResult
        argss.head.exprs
      case _ => return emptyResult
    }

    var minOpt: Option[BigInt] = None
    var maxOpt: Option[BigInt] = None
    var index: Boolean = false

    for (arg <- args) arg match {
      case arg: ScAssignStmt =>
        val an = arg.assignName
        val re = arg.getRExpression
        if (an.isEmpty || re.isEmpty) return emptyResult
        val name = arg.assignName.get
        name match {
          case "min" =>
            extractInt(re.get) match {
              case Some(n) => minOpt = Some(n)
              case _ => return emptyResult
            }
          case "max" =>
            extractInt(re.get) match {
              case Some(n) => maxOpt = Some(n)
              case _ => return emptyResult
            }
          case "index" =>
            extractBoolean(re.get) match {
              case Some(b) => index = b
              case _ => return emptyResult
            }
          case _ => return emptyResult
        }
      case _ => return emptyResult
    }

    var r = Vector[String]()

    val typeName = source.getName
    val iTermName = zCompanionName(typeName)
    val isTypeName = iSName(typeName)
    val msTypeName = mSName(typeName)
    val lowerTermName = scPrefix(typeName)
    val scTypeName = scName(typeName)

    lazy val (boxerType, boxerObject) = (minOpt, maxOpt) match {
      case (Some(min: BigInt), Some(max: BigInt)) =>
        if (scala.Byte.MinValue.toInt <= min && max.toInt <= scala.Byte.MaxValue)
          (s"$typeName.Boxer", s"object Boxer extends _root_.org.sireum.Z.Boxer.Byte { def make(o: $scalaPkg.Byte): $typeName }")
        else if (scala.Short.MinValue.toInt <= min && max.toInt <= scala.Short.MaxValue)
          (s"$typeName.Boxer", s"object Boxer extends _root_.org.sireum.Z.Boxer.Short { def make(o: $scalaPkg.Short): $typeName }")
        else if (scala.Int.MinValue <= min && max <= scala.Int.MaxValue)
          (s"$typeName.Boxer", s"object Boxer extends _root_.org.sireum.Z.Boxer.Int { def make(o: $scalaPkg.Int): $typeName }")
        else if (scala.Long.MinValue <= min && max <= scala.Long.MaxValue)
          (s"$typeName.Boxer", s"object Boxer extends _root_.org.sireum.Z.Boxer.Long { def make(o: $scalaPkg.Long): $typeName }")
        else (s"$sireumPkg.Z.Boxer.Z", "")
      case _ => (s"$sireumPkg.Z.Boxer.Z", "")
    }

    mode match {
      case Mode.Object =>

        r :+= s"def BitWidth: $scalaPkg.Int = ???"
        r :+= s"def random: $typeName = ???"
        r :+= s"def randomSeed(seed: $sireumPkg.Z): $typeName = ???"
        r :+= s"def apply(n: $scalaPkg.Int): $typeName = ???"
        r :+= s"def apply(n: $scalaPkg.Long): $typeName = ???"
        r :+= s"def apply(n: $sireumPkg.Z): $typeName = ???"
        r :+= s"def apply(n: $sireumString): $sireumPkg.Option[$typeName] = ???"
        r :+= s"def unapply(n: $typeName): $scalaPkg.Option[$sireumPkg.Z] = ???"
        r :+= s"implicit def to$scTypeName(sc: $scalaPkg.StringContext): $typeName.$scTypeName = ???"

      case Mode.Class =>

        r :+= s"override def value: $sireumPkg.Z = ???"
        r :+= s"override def make(v: $sireumPkg.Z): $typeName = ???"
        r :+= s"override def Name: $javaPkg.lang.String = ???"
        r :+= s"override def Min: $typeName = ???"
        r :+= s"override def Max: $typeName = ???"
        r :+= s"override def Index: $typeName = ???"
        r :+= s"override def isZeroIndex: $scalaPkg.Boolean = ???"
        r :+= s"override def isSigned: $scalaPkg.Boolean = ???"
        r :+= s"override def hasMin: $scalaPkg.Boolean = ???"
        r :+= s"override def hasMax: $scalaPkg.Boolean = ???"
        r :+= s"override def boxer: $boxerType = ???"

      case Mode.ObjectInners =>

        if (boxerObject != "") r :+= boxerObject

        r :+=
          s"""object Int extends $sireumPkg.$$ZCompanionInt[$typeName] {
             |  def apply(n: $scalaPkg.Int): $typeName = ???
             |  def unapply(n: $typeName): $scalaPkg.Option[$scalaPkg.Int] = ???
             |}
           """.stripMargin

        r :+=
          s"""object Long extends $sireumPkg.$$ZCompanionLong[$typeName] {
             |  def apply(n: $scalaPkg.Long): $typeName = ???
             |  def unapply(n: $typeName): $scalaPkg.Option[$scalaPkg.Long] = ???
             |}
           """.stripMargin

        r :+=
          s"""object $$String extends $sireumPkg.$$ZCompanionString[$typeName] {
             |  def apply(n: $javaPkg.lang.String): $typeName = ???
             |  def unapply(n: $typeName): $scalaPkg.Option[$javaPkg.lang.String] = ???
             |}
           """.stripMargin

        r :+=
          s"""object BigInt extends $sireumPkg.$$ZCompanionBigInt[$typeName] {
             |  def apply(n: $scalaPkg.BigInt): $typeName = ???
             |  def unapply(n: $typeName): $scalaPkg.Option[$scalaPkg.BigInt] = ???
             |}
           """.stripMargin

        r :+=
          s"""object $isTypeName {
             |  def apply[V <: $sireumPkg.Immutable](args: V*): $isTypeName[V] = $sireumPkg.IS[$typeName, V](args: _*)
             |  def create[V <: $sireumPkg.Immutable](size: $sireumPkg.Z, default: V): $isTypeName[V] = $sireumPkg.IS.create[$typeName, V](size, default)
             |}
           """.stripMargin

        r :+=
          s"""object $msTypeName {
             |  def apply[V](args: V*): $isTypeName[V] = $sireumPkg.MS[$typeName, V](args: _*)
             |  def create[V](size: $sireumPkg.Z, default: V): $isTypeName[V] = $sireumPkg.MS.create[$typeName, V](size, default)
             |}
           """.stripMargin

        r :+=
          s"""class $scTypeName(val sc: $scalaPkg.StringContext) {
             |  object $lowerTermName {
             |    def apply(args: $scalaPkg.Any*): $typeName = ???
             |    def unapply(n: $typeName): $scalaPkg.Boolean = ???
             |  }
             |}
           """.stripMargin

      case Mode.ObjectMembers =>

        r :+= s"type $isTypeName[T <: $sireumPkg.Immutable] = $sireumPkg.IS[$typeName, T]"
        r :+= s"type $msTypeName[T] = $sireumPkg.MS[$typeName, T]"
        r :+= s"val Name: $javaPkg.lang.String = ???"
        r :+= s"lazy val Min: $typeName = ???"
        r :+= s"lazy val Max: $typeName = ???"
        r :+= s"val Index: $typeName = ???"
        r :+= s"val isZeroIndex: $scalaPkg.Boolean = ???"
        r :+= s"val isSigned: $scalaPkg.Boolean = ???"
        r :+= s"val isBitVector: $scalaPkg.Boolean = ???"
        r :+= s"val hasMin: $scalaPkg.Boolean = ???"
        r :+= s"val hasMax: $scalaPkg.Boolean = ???"
        r :+= s"implicit val $iTermName: _root_.org.sireum.$$ZCompanion[$typeName]"
    }

    r
  }
}
