package net.rosien.sequentish

import cats.Show
import cats.implicits._

object MainToTerm extends App {
  showTerm[Int]
  showTerm[Nothing]
  showTerm[Unit]
  showTerm[(Int, String)]
  showTerm[(Int, String, Long)]
  showTerm[Either[Int, String]]
  showTerm[Int => String]
  showTerm[(Int, String) => Long] // TODO: this is inferred as (Int => String => Long)
  showTerm[Tuple2[Int, String] => Long]
  showTerm[Int => String => Long]
  showTerm[Int => String => Long => Double]
  showTerm[(Int, String, Long) => Double]
  // TODO: these don't infer
  // showTerm[(Nothing, Unit)]
  // showTerm[(Unit, Nothing)]

  def showTerm[A: ToTerm](): Unit =
    println(ToTerm[A].toTerm().show)
}

object MainProve extends App {

  trait A
  trait B
  trait C

  implicit val termA = ToTerm.reify[A]
  implicit val termB = ToTerm.reify[B]
  implicit val termC = ToTerm.reify[C]

  prove[LJT](Sequent[((A => A) => B) => B])

  println()
  prove[LJT](Sequent[((A => B) => C) => (A => B), (B => C) => (A => B)])
  println()
  prove[LJT](Sequent[(A, B, C) => (B, A)])
  println()
  prove[LJT](Sequent[(A, B), (B, A)]) // A * B = B * A
  prove[LJT](Sequent[Either[A, B], Either[B, A]]) // A + B = B + A
  prove[LJT](Sequent[(A, Either[B, C]), Either[(A, B), (A, C)]]) // A*(B+C) = (A*B)+(A*C)
  println()
  prove[LJT](Sequent[(Unit, A), A]) // 1 * A = A
  prove[LJT](Sequent[(A, Unit), A]) // A * 1 = A
  prove[LJT](Sequent[(Nothing, A), Nothing]) // 0 * A = 0
  prove[LJT](Sequent[(A, Nothing), Nothing]) // A * 0 = 0
  println()
  prove[LJT](Sequent[Either[Nothing, Nothing], Nothing]) // 0 + 0 = 0
  prove[LJT](Sequent[Either[A, Nothing], A]) // A + 0 = A
  prove[LJT](Sequent[Either[Nothing, A], A]) // 0 + A = A
  println()
  prove[LJT](Sequent[A => B => C, (A, B) => C]) // A => B => C = (A * B) => C
  prove[LJT](Sequent[Either[A, B] => C, (A => C, B => C)]) // (A + B) => C = (A => C) * (B => C)

  def prove[Rule: Prover: Show](sequent: Sequent): Unit =
    println(Prover[Rule].prove(sequent).prune.toTree.show)
}
