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

  implicit val toFormulaA = ToFormula.reify[A]
  implicit val toFormulaB = ToFormula.reify[B]
  implicit val toFormulaC = ToFormula.reify[C]

  printProof[LJT]("?", Sequent.conclude[((A => A) => B) => B])

  println()
  printProof[LJT]("?", Sequent[((A => B) => C) => (A => B), (B => C) => (A => B)])
  
  println()
  printProof[LJT]("∧-fst", Sequent[(A, B), A])
  printProof[LJT]("∧-snd", Sequent[(A, B), B])
  printProof[LJT]("∧-commutativity", Sequent[(A, B), (B, A)]) // A ∧ B = B ∧ A

  println()
  printProof[LJT]("∨-inl", Sequent[A, Either[A, B]])
  printProof[LJT]("∨-inr", Sequent[B, Either[A, B]])
  printProof[LJT]("∨-commutativity", Sequent[Either[A, B], Either[B, A]]) // A ∨ B = B ∨ A
  
  println()
  printProof[LJT]("∧-identity-l", Sequent[(Unit, A), A]) // 1 ∧ A = A
  printProof[LJT]("∧-identity-r", Sequent[(A, Unit), A]) // A ∧ 1 = A

  println()
  printProof[LJT]("∧-absorbsion-l", Sequent[(Nothing, A), Nothing]) // 0 ∧ A = 0
  printProof[LJT]("∧-absorbsion-r", Sequent[(A, Nothing), Nothing]) // A ∧ 0 = 0
  
  println()
  printProof[LJT]("∨-identity-l", Sequent[Either[Nothing, A], A]) // 0 ∨ A = A
  printProof[LJT]("∨-identity-r", Sequent[Either[A, Nothing], A]) // A ∨ 0 = A
  printProof[LJT]("∨-identity-inferred", Sequent[Either[Nothing, Nothing], Nothing]) // 0 ∨ 0 = 0
  
  println()
  // TODO: currying not provable(?)
  // prove[LJT](Sequent[A => B => C, Tuple2[A, B] => C]) // A => B => C = (A ∧ B) => C
  printProof[LJT]("∧∨-distributivity", Sequent[(A, Either[B, C]), Either[(A, B), (A, C)]]) // A∧(B∨C) = (A∧B)∨(A∧C)
  printProof[LJT]("∧∨-distributivity-exponentials", Sequent[Either[A, B] => C, (A => C, B => C)]) // (A ∨ B) => C = (A => C) ∧ (B => C)

  println()
  printProof[LJT]("const", Sequent.conclude[A => B => A])

  println()
  printProof[LK]("x", Sequent.conclude[Tuple2[A, B] => Either[A, C]])

  def printProof[Rule: Prover: Show](name: String, sequent: Sequent): Unit = {
    println(s"${Console.YELLOW}${Console.REVERSED}$name${Console.RESET}")
    println(Prover[Rule].prove(sequent).prune.toTree.show)
  }
}
