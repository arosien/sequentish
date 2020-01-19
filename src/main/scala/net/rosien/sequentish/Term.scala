package net.rosien.sequentish

import cats.Show
import cats.implicits._

sealed trait Term

object Term {
  sealed trait Atomic extends Term
  case object True extends Atomic
  case object False extends Atomic
  case class Type(name: String) extends Atomic
  case class And(fst: Term, snd: Term) extends Term
  case class Or(inl: Term, inr: Term) extends Term
  case class Implies(from: Term, to: Term) extends Term

  implicit val show: Show[Term] = {
    case True              => "⊤"
    case False             => "⊥"
    case Type(name)        => name
    case And(fst, snd)     => show"($fst ∧ $snd)"
    case Or(inl, inr)      => show"($inl ∨ $inr)"
    case Implies(from, to) => show"($from ⇒ $to)"
  }
}
