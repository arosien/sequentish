package net.rosien.sequentish

import cats.data.NonEmptyList

trait Deducer[Rule] {
  def deduce(rule: Rule): Sequent => Deduction[Rule]
}

object Deducer {
  implicit def apply[Rule](implicit d: Deducer[Rule]): Deducer[Rule] = d

  def fromPFs[Rule](definedRules: Rule => PartialFunction[Sequent, Deduction[Rule]]): Deducer[Rule] =
    new Deducer[Rule] {
      def deduce(rule: Rule): Sequent => Deduction[Rule] =
        s =>
          definedRules(rule)
            .applyOrElse(s, (_: Sequent) => Deduction.Stuck(rule))
    }
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
