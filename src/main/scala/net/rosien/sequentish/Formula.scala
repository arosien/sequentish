package net.rosien.sequentish

import cats.Show
import cats.implicits._

// tag::all[]
// tag::formula[]
sealed trait Formula
// end::formula[]

object Formula {
  sealed trait Atomic extends Formula // <1>
  case object True extends Atomic
  case object False extends Atomic
  case class Type(name: String) extends Atomic
  case class And(fst: Formula, snd: Formula) extends Formula
  case class Or(inl: Formula, inr: Formula) extends Formula
  case class Implies(from: Formula, to: Formula) extends Formula
// end::all[]
// tag::show[]
  implicit val show: Show[Formula] = {
    case True              => "⊤"
    case False             => "⊥"
    case Type(name)        => name
    case And(fst, snd)     => show"($fst ∧ $snd)"
    case Or(inl, inr)      => show"($inl ∨ $inr)"
    case Implies(from, to) => show"($from ⇒ $to)"
  }
  // end::show[]  

  // TODO: more generic extractors?
  // TODO: perhaps produce disjunctions of conjunctions for multiple matches?
  object firstAnd {
    def unapply(terms: List[Formula]): Option[(Formula.And, List[Formula])] =
      terms.collectFirst { case t: Formula.And => t -> terms.diff(List(t)) }
  }
  object firstOr {
    def unapply(terms: List[Formula]): Option[(Formula.Or, List[Formula])] =
      terms.collectFirst { case t: Formula.Or => t -> terms.diff(List(t)) }
  }
  // tag::all[]
}
// end::all[]