package net.rosien.sequentish

import cats.data.NonEmptyList

sealed trait Proof2[R]

object Proof2 {
  case class Root[R](sequent: Sequent, any: NonEmptyList[RuleProof[R]])
      extends Proof2[R]
  sealed trait RuleProof[R] extends Proof2[R]
  case class Stuck[R](rule: R) extends RuleProof[R]
  case class Discharged[R](rule: R) extends RuleProof[R]
  case class And[R](rule: R, proofs: NonEmptyList[Proof2[R]])
      extends RuleProof[R]
}
