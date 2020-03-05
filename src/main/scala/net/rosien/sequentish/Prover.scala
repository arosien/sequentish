package net.rosien.sequentish

import cats.data._
import cats.implicits._

/** A [[System]] contains a set of rules. */
case class System[Rule](rules: NonEmptySet[Rule])

trait Deducer[Rule] {
  def deduce(rule: Rule): PartialFunction[Sequent, Deduction[Rule]]
}

object Deducer {
  implicit def apply[Rule](implicit d: Deducer[Rule]): Deducer[Rule] = d
}

sealed trait Deduction[Rule]

object Deduction {

  /** No sequents could be deduced. */
  case class Stuck[Rule](rule: Rule) extends Deduction[Rule]

  /** The sequent is aximatically true, so it may be discharged (proved). */
  case class Discharged[Rule](rule: Rule) extends Deduction[Rule]

  /** The deduction was successful and produced new sequents. */
  case class Success[Rule](rule: Rule, sequents: NonEmptyList[Sequent])
      extends Deduction[Rule]
}

/** Prove [[Sequent]]s using a set of rules. */
trait Prover[Rule] {
  def prove(sequent: Sequent): Proof[Rule]
}

object Prover {
  def apply[Rule](implicit p: Prover[Rule]): Prover[Rule] = p

  def apply[Rule: Deducer](system: System[Rule]): Prover[Rule] =
    new Prover[Rule] {
      def prove(sequent: Sequent): Proof[Rule] =
        Proof.Root(
          sequent,
          system.rules.reduceMap(
            rule =>
              NonEmptyList.one(
                prove(
                  PartialFunction
                    .condOpt(sequent)(Deducer[Rule].deduce(rule))
                    .getOrElse(Deduction.Stuck(rule))
                )
              )
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
