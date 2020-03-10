package net.rosien.sequentish

import cats.data._
import cats.implicits._

/** Prove [[Formula]] or [[Sequent]]s using a set of rules. */
// tag::prover[]
trait Prover[Rule] {
  def prove(sequent: Sequent): Proof[Rule]
  // end::prover[]

  def prove(formula: Formula): Proof[Rule] =
    prove(Sequent.conclude(formula))
// tag::prover[]
}
// end::prover[]

object Prover {
  def apply[Rule](implicit p: Prover[Rule]): Prover[Rule] = p

  def apply[Rule: Deducer](system: System[Rule]): Prover[Rule] =
    new Prover[Rule] {
      def prove(sequent: Sequent): Proof[Rule] =
        Proof.Root(
          sequent,
          system.rules.reduceMap(
            rule => NonEmptyList.one(prove(Deducer[Rule].deduce(rule)(sequent)))
          )
        )

      def prove(deduction: Deduction[Rule]): RuleProof[Rule] =
        deduction match {
          case Deduction.Stuck(rule)             => RuleProof.Stuck(rule)
          case Deduction.Discharged(rule)        => RuleProof.Discharged(rule)
          case Deduction.Success(rule, sequents) =>
            // TODO: short-circuit after any (recursive) failure
            RuleProof.And(rule, sequents.map(prove))
        }
    }
}
