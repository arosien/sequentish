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

// tag::deduction[]
sealed trait Deduction[Rule]

// end::deduction[]

object Deduction {

  /** No sequents could be deduced. */
  // tag::deduction[]
  case class Stuck[Rule](rule: Rule) extends Deduction[Rule]
  
  // end::deduction[]
  /** The sequent is aximatically true, so it may be discharged (proved). */
  // tag::deduction[]
  case class Discharged[Rule](rule: Rule) extends Deduction[Rule]
  
  // end::deduction[]
  /** The deduction was successful and produced new sequents. */
  // tag::deduction[]
  case class Success[Rule](rule: Rule, sequents: NonEmptyList[Sequent])
      extends Deduction[Rule]
  // end::deduction[]
}
