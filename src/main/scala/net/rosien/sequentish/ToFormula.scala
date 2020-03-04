package net.rosien.sequentish
import scala.reflect.ClassTag

// tag::ToFormula-def[]
trait ToFormula[A] {
  def toFormula(): Formula
}
// end::ToFormula-def[]

object ToFormula {
  def apply[A](implicit to: ToFormula[A]): ToFormula[A] = to

  def reify[A](implicit ct: ClassTag[A]): ToFormula[A] =
    () => Formula.Type(ct.runtimeClass.getSimpleName())

  def fromType[A: ToFormula]: Formula = ToFormula[A].toFormula

  implicit val one: ToFormula[Unit] =
    () => Formula.True

  implicit val zero: ToFormula[Nothing] =
    () => Formula.False

  implicit def product2_0F[A <: Nothing, B](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[(A, B)] =
    product2[A, B](toA, toB)

  implicit def product2_0S[A, B <: Nothing](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[(A, B)] =
    product2[A, B](toA, toB)

  implicit def product2[A, B](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[(A, B)] =
    () => Formula.And(toA.toFormula(), toB.toFormula())

  implicit def product3[A, B, C](
      implicit toA: ToFormula[A],
      toBC: ToFormula[(B, C)]
  ): ToFormula[(A, B, C)] =
    () => Formula.And(toA.toFormula(), toBC.toFormula())

  implicit def sum00[A <: Nothing, B <: Nothing](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[Either[A, B]] =
    sum[A, B](toA, toB)

  implicit def sum0L[A <: Nothing, B](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[Either[A, B]] =
    sum[A, B](toA, toB)

  implicit def sum0R[A, B <: Nothing](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[Either[A, B]] =
    sum[A, B](toA, toB)

  implicit def sum[A, B](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[Either[A, B]] =
    () => Formula.Or(toA.toFormula(), toB.toFormula())

  implicit def implies[A, B](
      implicit toA: ToFormula[A],
      toB: ToFormula[B]
  ): ToFormula[A => B] =
    () => Formula.Implies(toA.toFormula(), toB.toFormula())

  implicit def implies2[A, B, C](
      implicit toABC: ToFormula[A => B => C]
  ): ToFormula[(A, B) => C] =
    () => toABC.toFormula()

  implicit def implies3[A, B, C, D](
      implicit toABCD: ToFormula[A => B => C => D]
  ): ToFormula[(A, B, C) => D] =
    () => toABCD.toFormula()

  implicit val int: ToFormula[Int] =
    () => Formula.Type("Int")
  implicit val string: ToFormula[String] =
    () => Formula.Type("String")
  implicit val long: ToFormula[Long] =
    () => Formula.Type("Long")
  implicit val double: ToFormula[Double] =
    () => Formula.Type("Double")
}
