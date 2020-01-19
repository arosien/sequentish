package net.rosien.sequentish

import cats._
import cats.data._
import cats.implicits._
import higherkindness.droste.data._
import higherkindness.droste.util.DefaultTraverse

case class ProofF[R, A](sequent: Sequent, any: NonEmptyList[A])

object ProofF {

  implicit def traverse[R]: Traverse[ProofF[R, ?]] =
    new DefaultTraverse[ProofF[R, ?]] {
      def traverse[G[_]: Applicative, A, B](
          fa: ProofF[R, A]
      )(f: A => G[B]): G[ProofF[R, B]] = fa match {
        case ProofF(sequent, any) => any.traverse(f).map(ProofF(sequent, _))
      }
    }

  type Fixed[R] = Fix[ProofF[R, ?]]
}

sealed trait RuleF[R, A]

object RuleF {
  case class Stuck[R, A](rule: R) extends RuleF[R, A]
  case class Discharged[R, A](rule: R) extends RuleF[R, A]
  case class And[R, A](rule: R, all: NonEmptyList[A]) extends RuleF[R, A]

  def traverse[R]: Traverse[RuleF[R, ?]] = new DefaultTraverse[RuleF[R, ?]] {
    def traverse[G[_]: Applicative, A, B](
        fa: RuleF[R, A]
    )(f: A => G[B]): G[RuleF[R, B]] =
      fa match {
        case Stuck(rule)      => (Stuck[R, B](rule): RuleF[R, B]).pure[G]
        case Discharged(rule) => (Discharged[R, B](rule): RuleF[R, B]).pure[G]
        case And(rule, all)   => all.traverse(f).map(And(rule, _))
      }
  }

  type Fixed[R] = Fix[RuleF[R, ?]]
}

trait ProverF {
  type Rule
  def rules(): NonEmptySet[Rule]

}
