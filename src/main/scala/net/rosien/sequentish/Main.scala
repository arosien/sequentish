package net.rosien.sequentish

import cats.Show
import cats.implicits._

object MainToFormula extends App {
  printFormula[Int]
  printFormula[Nothing]
  printFormula[Unit]
  printFormula[(Int, String)]
  printFormula[(Int, String, Long)]
  printFormula[Either[Int, String]]
  printFormula[Int => String]
  printFormula[(Int, String) => Long] // TODO: this is inferred as (Int => String => Long)
  printFormula[Tuple2[Int, String] => Long]
  printFormula[Int => String => Long]
  printFormula[Int => String => Long => Double]
  printFormula[(Int, String, Long) => Double]
  printFormula[Tuple3[Int, String, Long] => Double]
  // TODO: these don't infer
  // showTerm[(Nothing, Unit)]
  // showTerm[(Unit, Nothing)]

  def printFormula[A: ToFormula](): Unit =
    println(ToFormula[A].toFormula().show)
}

object MainProve extends App {

  trait A
  trait B
  trait C

  implicit val termA = ToFormula.reify[A]
  implicit val termB = ToFormula.reify[B]
  implicit val termC = ToFormula.reify[C]

  printProof[LJT](Sequent[((A => A) => B) => B])

  println()
  printProof[LJT](Sequent[((A => B) => C) => (A => B), (B => C) => (A => B)])
  println()
  printProof[LJT](Sequent[(A, B, C) => (B, A)])
  println()
  printProof[LJT](Sequent[(A, B), (B, A)]) // A * B = B * A
  printProof[LJT](Sequent[Either[A, B], Either[B, A]]) // A + B = B + A
  printProof[LJT](Sequent[(A, Either[B, C]), Either[(A, B), (A, C)]]) // A*(B+C) = (A*B)+(A*C)
  println()
  printProof[LJT](Sequent[(Unit, A), A]) // 1 * A = A
  printProof[LJT](Sequent[(A, Unit), A]) // A * 1 = A
  printProof[LJT](Sequent[(Nothing, A), Nothing]) // 0 * A = 0
  printProof[LJT](Sequent[(A, Nothing), Nothing]) // A * 0 = 0
  println()
  printProof[LJT](Sequent[Either[Nothing, Nothing], Nothing]) // 0 + 0 = 0
  printProof[LJT](Sequent[Either[A, Nothing], A]) // A + 0 = A
  printProof[LJT](Sequent[Either[Nothing, A], A]) // 0 + A = A
  println()
  // TODO: currying not provable(?)
  // prove[LJT](Sequent[A => B => C, Tuple2[A, B] => C]) // A => B => C = (A * B) => C
  printProof[LJT](Sequent[Either[A, B] => C, (A => C, B => C)]) // (A + B) => C = (A => C) * (B => C)
  println()
  printProof[LJT](Sequent[A])

  def printProof[Rule: Prover: Show](sequent: Sequent): Unit =
    println(Prover[Rule].prove(sequent).prune.toTree.show)
}
