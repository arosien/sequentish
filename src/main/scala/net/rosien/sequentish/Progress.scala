package net.rosien.sequentish

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.util.DefaultTraverse

sealed trait Progress[V, A]

object Progress {
  case class Stuck[V, A](rule: V) extends Progress[V, A]
  case class Discharged[V, A](rule: V) extends Progress[V, A]
  case class Next[V, A](rule: V, next: NonEmptyList[A]) extends Progress[V, A]
  case class Any[V, A](sequent: Sequent, any: NonEmptyList[A])
      extends Progress[V, A]

  type Fixed[V] = Fix[Progress[V, ?]]
}

object ProgressMain extends App {

  implicit def traverse[V]: Traverse[Progress[V, ?]] =
    new DefaultTraverse[Progress[V, ?]] {
      def traverse[G[_]: Applicative, A, B](
          fa: Progress[V, A]
      )(f: A => G[B]): G[Progress[V, B]] =
        fa match {
          case Progress.Stuck(rule) =>
            (Progress.Stuck(rule): Progress[V, B]).pure[G]
          case Progress.Discharged(rule) =>
            (Progress.Discharged(rule): Progress[V, B]).pure[G]
          case Progress.Next(rule, next) =>
            next.traverse(f).map(Progress.Next(rule, _))
        }
    }

  type Deduction[V] = Progress[V, Sequent]

  def derive[Rule: Deducer](
      system: System[Rule]
  ): Coalgebra[Progress[Rule, ?], Sequent] =
    Coalgebra[Progress[Rule, ?], Sequent] { sequent =>
      Progress.Any(
        sequent,
        system.rules.reduceMap(
          rule =>
            NonEmptyList.one(
              PartialFunction
                .condOpt(sequent)(Deducer[Rule].deduce(rule))
                .getOrElse(Deduction.Stuck(rule)) match {
                case Deduction.Stuck(rule)      => Progress.Stuck(sequent)
                case Deduction.Discharged(rule) => Progress.Discharged(sequent)
                case Deduction.Success(rule, sequents) =>
                  Progress.Next(rule, sequents)
              }
            )
        )
      )
    }

  val A = Formula.Type("A")

  val proof: Progress.Fixed[(LJT, Sequent)] =
    Fix(
      Progress.Next(
        LJT.`R⇒` -> Sequent(A, A),
        NonEmptyList.of(Fix(Progress.Discharged(LJT.Id -> Sequent(A, A))))
      )
    )
}
