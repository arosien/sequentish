package net.rosien.sequentish

import cats.Show
import cats.implicits._

object Main extends App {
  println(ToTerm[Int].toTerm().show)
  println(ToTerm[(Int, String)].toTerm().show)
  println(ToTerm[(Int, String, Long)].toTerm().show)
  println(ToTerm[Either[Int, String]].toTerm().show)
  println(ToTerm[Int => String].toTerm().show)
  println(ToTerm[(Int, String) => Long].toTerm().show)
  println(ToTerm[Int => String => Long].toTerm().show)
  println(ToTerm[Int => String => Long => Double].toTerm().show)
  println(ToTerm[(Int, String, Long) => Double].toTerm().show)

  trait R
  trait Q
  implicit val rt: ToTerm[R] = ToTerm.reify
  implicit val qt: ToTerm[Q] = ToTerm.reify
  
  println("")
  prove[LJT](Sequent[((R => R) => Q) => Q])
  
  trait A
  trait B
  trait C
  implicit val at: ToTerm[A] = ToTerm.reify
  implicit val bt: ToTerm[B] = ToTerm.reify
  implicit val ct: ToTerm[C] = ToTerm.reify
  
  println()
  prove[LJT](Sequent[((A => B) => C) => (A => B), (B => C) => (A => B)])
  println()
  prove[LJT](Sequent[(A, B, C) => (B, A)])
  println()
  prove[LJT](Sequent[(A, B), (B, A)])
  println()
  prove[LJT](Sequent[(A, Either[B, C]), Either[(A, B), (A, C)]])

  def prove[Rule: Prover: Show](sequent: Sequent): Unit =
    println(Prover[Rule].prove(sequent).prune.toTree.show)
}
