package net.rosien.sequentish
import scala.reflect.ClassTag

trait ToTerm[A] {
  def toTerm(): Term
}

object ToTerm {
  def apply[A](implicit to: ToTerm[A]): ToTerm[A] = to

  def reify[A](implicit ct: ClassTag[A]): ToTerm[A] =
    () => Term.Type(ct.runtimeClass.getSimpleName())

  def term[A: ToTerm]: Term = ToTerm[A].toTerm

  implicit val one: ToTerm[Unit] =
    () => Term.True

  implicit val zero: ToTerm[Nothing] =
    () => Term.False

  implicit def product2_0F[A <: Nothing, B](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[(A, B)] =
    product2[A, B](toA, toB)

  implicit def product2_0S[A, B <: Nothing](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[(A, B)] =
    product2[A, B](toA, toB)

  implicit def product2[A, B](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[(A, B)] =
    () => Term.And(toA.toTerm(), toB.toTerm())

  implicit def product3[A, B, C](
      implicit toA: ToTerm[A],
      toBC: ToTerm[(B, C)]
  ): ToTerm[(A, B, C)] =
    () => Term.And(toA.toTerm(), toBC.toTerm())

  implicit def sum00[A <: Nothing, B <: Nothing](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[Either[A, B]] =
    sum[A, B](toA, toB)

  implicit def sum0L[A <: Nothing, B](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[Either[A, B]] =
    sum[A, B](toA, toB)

  implicit def sum0R[A, B <: Nothing](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[Either[A, B]] =
    sum[A, B](toA, toB)

  implicit def sum[A, B](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[Either[A, B]] =
    () => Term.Or(toA.toTerm(), toB.toTerm())

  implicit def implies[A, B](
      implicit toA: ToTerm[A],
      toB: ToTerm[B]
  ): ToTerm[A => B] =
    () => Term.Implies(toA.toTerm(), toB.toTerm())

  implicit def implies2[A, B, C](
      implicit toABC: ToTerm[A => B => C]
  ): ToTerm[(A, B) => C] =
    () => toABC.toTerm()

  implicit def implies3[A, B, C, D](
      implicit toABCD: ToTerm[A => B => C => D]
  ): ToTerm[(A, B, C) => D] =
    () => toABCD.toTerm()

  implicit val int: ToTerm[Int] =
    () => Term.Type("Int")
  implicit val string: ToTerm[String] =
    () => Term.Type("String")
  implicit val long: ToTerm[Long] =
    () => Term.Type("Long")
  implicit val double: ToTerm[Double] =
    () => Term.Type("Double")
}
