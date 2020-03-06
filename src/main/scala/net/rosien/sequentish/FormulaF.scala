package net.rosien.sequentish

import cats._
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.syntax.all._
import higherkindness.droste.util.DefaultTraverse

sealed trait FormulaF[A]

object FormulaF {
  sealed trait Atomic[A] extends FormulaF[A]
  case class True[A]() extends Atomic[A]
  case class False[A]() extends Atomic[A]
  case class Type[A](name: String) extends Atomic[A]
  case class And[A](fst: A, snd: A) extends FormulaF[A]
  case class Or[A](inl: A, inr: A) extends FormulaF[A]
  case class Implies[A](from: A, to: A) extends FormulaF[A]

  type Fixed = Fix[FormulaF[?]]

  implicit val traverse: Traverse[FormulaF] = new DefaultTraverse[FormulaF] {
    def traverse[G[_]: Applicative, A, B](fa: FormulaF[A])(
        f: A => G[B]
    ): G[FormulaF[B]] =
      fa match {
        case True()            => (True(): FormulaF[B]).pure[G]
        case False()           => (False(): FormulaF[B]).pure[G]
        case Type(name)        => (Type(name): FormulaF[B]).pure[G]
        case And(fst, snd)     => (f(fst), f(snd)).mapN(And.apply)
        case Or(inl, inr)      => (f(inl), f(inr)).mapN(Or.apply)
        case Implies(from, to) => (f(from), f(to)).mapN(Implies.apply)
      }
  }

  val inhabitants: Fixed => BigInt =
    scheme cata Algebra[FormulaF, BigInt] {
      case True()            => 1
      case False()           => 0
      case Type(_)           => 1 // TODO: wrong
      case And(fst, snd)     => fst * snd
      case Or(inl, inr)      => inl + inr
      case Implies(from, to) => to.pow(from.toInt)
    }

  val toTreeTrans =
    Trans[FormulaF, RoseF[String, ?], RoseF.Fixed[String]] {
      case True()            => RoseF("⊤", Nil)
      case False()           => RoseF("⊥", Nil)
      case Type(name)        => RoseF(name, Nil)
      case And(fst, snd)     => RoseF("∧", List(fst, snd))
      case Or(inl, inr)      => RoseF("∨", List(inl, inr))
      case Implies(from, to) => RoseF("⇒", List(from, to))
    }

  val toTree: FormulaF.Fixed => RoseF.Fixed[String] =
    scheme cata toTreeTrans.algebra

  trait FormulaFImplicits {
    implicit val basis: Basis[FormulaF, Formula] =
      Basis.Default(
        Algebra {
          case True()            => Formula.True
          case False()           => Formula.False
          case Type(name)        => Formula.Type(name)
          case And(fst, snd)     => Formula.And(fst, snd)
          case Or(inl, inr)      => Formula.Or(inl, inr)
          case Implies(from, to) => Formula.Implies(from, to)
        },
        Coalgebra {
          case Formula.True              => True()
          case Formula.False             => False()
          case Formula.Type(name)        => Type(name)
          case Formula.And(fst, snd)     => And(fst, snd)
          case Formula.Or(inl, inr)      => Or(inl, inr)
          case Formula.Implies(from, to) => Implies(from, to)
        }
      )
  }
}

object FormulaFExample extends App {
  val t0: FormulaF.Fixed =
    FormulaF
      .And(
        FormulaF.True().fix[FormulaF],
        FormulaF
          .Or(
            FormulaF.False().fix[FormulaF],
            FormulaF
              .Implies(FormulaF.Type("A").fix[FormulaF], FormulaF.Type("B").fix[FormulaF])
              .fix[FormulaF]
          )
          .fix[FormulaF]
      )
      .fix[FormulaF]
  val tree0 = FormulaF.toTree(t0)
  println(RoseF.print(tree0))

  println()

  println(RoseF.pretty2[String](2).apply(tree0))

  def prettyAlg[V: Show](indent: Int = 2): RoseF.PrettyAlg[FormulaF] =
    Algebra[AttrF[FormulaF, Int, ?], String] {
      case AttrF(depth, FormulaF.True()) =>
        val prefix = (" " * indent) * depth
        show"$prefix⊤"
      case AttrF(depth, FormulaF.False()) =>
        val prefix = (" " * indent) * depth
        show"$prefix⊤"
      case AttrF(depth, FormulaF.Type(name)) =>
        val prefix = (" " * indent) * depth
        show"$prefix$name"
      case AttrF(depth, FormulaF.And(fst, snd)) =>
        val prefix = (" " * indent) * depth
        show"$prefix(∧\n$fst\n$snd)"
      case AttrF(depth, FormulaF.Or(inl, inr)) =>
        val prefix = (" " * indent) * depth
        show"$prefix(∨\n$inl\n$inr)"
      case AttrF(depth, FormulaF.Implies(from, to)) =>
        val prefix = (" " * indent) * depth
        show"$prefix(⇒\n$from\n$to)"
    }

  println(RoseF.pretty(prettyAlg[String]()).apply(t0))
}
