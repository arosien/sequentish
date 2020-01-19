package net.rosien.sequentish

import cats.Show
import cats.data._
import cats.implicits._

/** Prove [[Sequent]]s using a set of rules. */
trait Prover[A] {

  /** Rules available for proving. */
  def rules(): NonEmptySet[A]

  /** Attempt to prove the sequent using one rule. */
  def prove(
      rule: A,
      from: Sequent
  ): Prover.Step[A]

  def prove(sequent: Sequent): Prover.Proof[A] =
    Prover.Proof(sequent, rules.toNonEmptyList, prove(_, _).toRuleProof(this))
}

object Prover {
  def apply[A](implicit p: Prover[A]): Prover[A] = p

  sealed trait Proof[A] {
    lazy val proved: Boolean =
      this match {
        case Proof.Root(_, any) => any.exists(_.proved)
      }

    def prune(): Proof[A] =
      this match {
        case Proof.Root(sequent, any) =>
          Proof.Root(
            sequent,
            any
              .map(_.prune)
              .find(_.proved) // choose the first proved of the disjunction
              .toList
              .toNel
              .getOrElse(any.map(_.prune))
          )
      }

    def toTree(): Tree[Proof.Node[A]] = {
      def toTree(rp: RuleProof[A]): Tree[Proof.Node[A]] =
        rp match {
          case RuleProof.Discharged(rule) => Tree(Proof.Node.D(rule), Nil)
          case RuleProof.Stuck(rule)      => Tree(Proof.Node.X(rule), Nil)
          case RuleProof.And(rule, proofs) =>
            Tree(Proof.Node.R(rule), proofs.map(_.toTree).toList)
        }

      this match {
        case Proof.Root(sequent, any) =>
          Tree(Proof.Node.S(sequent), any.map(toTree).toList)
      }
    }
  }

  object Proof {
    case class Root[A](sequent: Sequent, any: NonEmptyList[RuleProof[A]])
        extends Proof[A]

    def apply[A](
        sequent: Sequent,
        rules: NonEmptyList[A],
        f: (A, Sequent) => RuleProof[A]
    ): Proof[A] =
      Root(
        sequent,
        rules.reduceMap(rule => NonEmptyList.one(f(rule, sequent)))
      )

    implicit def show[A: Show]: Show[Proof[A]] = {
      case p @ Root(sequent, any) =>
        if (p.proved)
          show"(${Console.GREEN}$sequent, (${any.mkString_(" ∨ ")}))"
        else
          show"(${Console.YELLOW}$sequent${Console.RESET}, (${any.mkString_(" ∨ ")}))"
    }

    sealed trait Node[A]
    object Node {
      case class S[A](sequent: Sequent) extends Node[A]
      case class R[A](rule: A) extends Node[A]
      case class D[A](rule: A) extends Node[A]
      case class X[A](rule: A) extends Node[A]

      implicit def show[A: Show]: Show[Node[A]] = {
        case S(s) => s.show
        case R(r) => r.show
        case D(r) => show"${Console.GREEN}$r${Console.RESET}"
        case X(r) => show"${Console.RED}$r${Console.RESET}"
      }
    }
  }

  sealed trait RuleProof[A] {
    def rule: A

    lazy val proved: Boolean =
      this match {
        case RuleProof.Stuck(_)       => false
        case RuleProof.Discharged(_)  => true
        case RuleProof.And(_, proofs) => proofs.forall(_.proved)
      }

    def prune: RuleProof[A] =
      this match {
        case RuleProof.Stuck(_) | RuleProof.Discharged(_) => this
        case RuleProof.And(rule, proofs) =>
          RuleProof.And(rule, proofs.map(_.prune()))
      }
  }

  object RuleProof {
    case class Stuck[A](rule: A) extends RuleProof[A]
    case class Discharged[A](rule: A) extends RuleProof[A]
    case class And[A](rule: A, proofs: NonEmptyList[Proof[A]])
        extends RuleProof[A]

    implicit def show[A: Show]: Show[RuleProof[A]] = {
      case Stuck(rule)      => show"${Console.RED}$rule${Console.RESET}"
      case Discharged(rule) => show"${Console.GREEN}$rule${Console.RESET}"
      case rp @ And(rule, proofs) =>
        if (rp.proved)
          proofs.mkString_(show"(${Console.GREEN}$rule, (", " ∧ ", "))")
        else proofs.mkString_(show"($rule, (", " ∧ ", "))")
    }
  }

  sealed trait Step[A] {
    def rule: A

    def toRuleProof(implicit P: Prover[A]): RuleProof[A] =
      this match {
        case Step.Stuck(rule)      => RuleProof.Stuck(rule)
        case Step.Discharged(rule) => RuleProof.Discharged(rule)
        case Step.And(rule, sequents) =>
          // TODO: short-circuit after any (recursive) failure
          RuleProof.And(rule, sequents.map(P.prove))
      }
  }

  object Step {
    case class Stuck[A](rule: A) extends Step[A]
    case class Discharged[A](rule: A) extends Step[A]
    case class And[A](rule: A, sequents: NonEmptyList[Sequent]) extends Step[A]

    def and[A](rule: A, sequent: Sequent, sequents: Sequent*): Step[A] = And(rule, NonEmptyList.of(sequent, (sequents: _*)))

    def apply[A](rule: A, sequent: Sequent)(
        pf: PartialFunction[Sequent, Step[A]]
    ): Step[A] =
      PartialFunction.condOpt(sequent)(pf).getOrElse(Stuck(rule))
  }
}
