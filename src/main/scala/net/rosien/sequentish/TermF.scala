package net.rosien.sequentish

import cats._
import cats.data.State
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.data.prelude._
import higherkindness.droste.syntax.all._
import higherkindness.droste.util.DefaultTraverse

sealed trait TermF[A]

object TermF {
  sealed trait Atomic[A] extends TermF[A]
  case class True[A]() extends Atomic[A]
  case class False[A]() extends Atomic[A]
  case class Type[A](name: String) extends Atomic[A]
  case class And[A](fst: A, snd: A) extends TermF[A]
  case class Or[A](inl: A, inr: A) extends TermF[A]
  case class Implies[A](from: A, to: A) extends TermF[A]

  type Fixed = Fix[TermF[?]]

  implicit val traverse: Traverse[TermF] = new DefaultTraverse[TermF] {
    def traverse[G[_]: Applicative, A, B](fa: TermF[A])(
        f: A => G[B]
    ): G[TermF[B]] =
      fa match {
        case True()            => (True(): TermF[B]).pure[G]
        case False()           => (False(): TermF[B]).pure[G]
        case Type(name)        => (Type(name): TermF[B]).pure[G]
        case And(fst, snd)     => (f(fst), f(snd)).mapN(And.apply)
        case Or(inl, inr)      => (f(inl), f(inr)).mapN(Or.apply)
        case Implies(from, to) => (f(from), f(to)).mapN(Implies.apply)
      }
  }

  val inhabitants: Fixed => BigInt =
    scheme cata Algebra[TermF, BigInt] {
      case True()            => 1
      case False()           => 0
      case Type(_)           => 1 // TODO: wrong
      case And(fst, snd)     => fst * snd
      case Or(inl, inr)      => inl + inr
      case Implies(from, to) => to.pow(from.toInt)
    }

  val toTreeTrans =
    Trans[TermF, RoseF[String, ?], RoseF.Fixed[String]] {
      case True()            => RoseF("⊤", Nil)
      case False()           => RoseF("⊥", Nil)
      case Type(name)        => RoseF(name, Nil)
      case And(fst, snd)     => RoseF("∧", List(fst, snd))
      case Or(inl, inr)      => RoseF("∨", List(inl, inr))
      case Implies(from, to) => RoseF("⇒", List(from, to))
    }

  val toTree: TermF.Fixed => RoseF.Fixed[String] =
    scheme cata toTreeTrans.algebra

  trait TermFImplicits {
    implicit val basis: Basis[TermF, Term] =
      Basis.Default(
        Algebra {
          case True()            => Term.True
          case False()           => Term.False
          case Type(name)        => Term.Type(name)
          case And(fst, snd)     => Term.And(fst, snd)
          case Or(inl, inr)      => Term.Or(inl, inr)
          case Implies(from, to) => Term.Implies(from, to)
        },
        Coalgebra {
          case Term.True              => True()
          case Term.False             => False()
          case Term.Type(name)        => Type(name)
          case Term.And(fst, snd)     => And(fst, snd)
          case Term.Or(inl, inr)      => Or(inl, inr)
          case Term.Implies(from, to) => Implies(from, to)
        }
      )
  }
}

case class RoseF[V, A](root: V, children: List[A])

object RoseF {
  type Fixed[V] = Fix[RoseF[V, ?]]

  implicit def traverse[V]: Traverse[RoseF[V, ?]] =
    new DefaultTraverse[RoseF[V, ?]] {
      def traverse[G[_]: Applicative, A, B](
          fa: RoseF[V, A]
      )(f: A => G[B]): G[RoseF[V, B]] =
        fa match {
          case RoseF(root, children) => children.traverse(f).map(RoseF(root, _))
        }
    }

  def inc[V] = CoalgebraM[State[Int, ?], RoseF[V, ?], Fixed[V]] {
    case Fix(RoseF(root, children)) =>
      for {
        _ <- State.modify[Int](_ + 1)
      } yield RoseF(root, children)
  }
  def dec[V] = AlgebraM[State[Int, ?], RoseF[V, ?], Fixed[(V, Int)]] {
    case RoseF(root, children) =>
      for {
        d <- State.get[Int]
        _ <- State.modify[Int](_ - 1)
      } yield RoseF(root -> d, children).fix
  }
  def h[V] = scheme.hyloM(dec[V], inc[V])

  def print[V: Show](f: Fixed[V]): String =
    scheme
      .cata(Algebra[RoseF[V, ?], String] {
        case RoseF(root, children) =>
          if (children.isEmpty) root.show
          else show"($root, (${children.mkString(", ")}))"
      })
      .apply(f)

  def prettyAlg[V: Show](indent: Int = 2) =
    Algebra[RoseF[(V, Int), ?], String] {
      case RoseF((root, depth), children) =>
        val prefix = (" " * indent) * depth
        if (children.isEmpty) show"$prefix$root"
        else show"$prefix($root${children.mkString("\n", "\n", "")})"
    }

  def pretty[V: Show](f: Fixed[V], indent: Int = 2): String =
    scheme
      .cata(prettyAlg[V](indent))
      .apply(h(f).runA(-1).value)

  /*
  > inherit :: forall f a. Functor f =>
  >             (Fix f -> a -> a) -> a -> Fix f -> Ann f a
  > inherit f root n = para alg n root where
  >   alg :: f (a -> Ann f a, Fix f) -> (a -> Ann f a)
  >   alg (funzip -> (ff, n)) p = ann (n', a)
  >     where
  >       a  = f (Fix n) p
  >       n' = fmap ($ a) ff
   */
  def inherit[F[_]: Functor, R](
      f: Fix[F] => R => R,
      root: R,
      n: Fix[F]
  ): Attr[F, R] =
    scheme.zoo
      .para(RAlgebra[Fix[F], F, R => Attr[F, R]] { ffn => r1 =>
        {
          // ff: F[R => Attr[F, R]]
          // n2: F[Fix[F]]
          val (n2, ff) = (ffn.map(_._1), ffn.map(_._2))
          val r: R => R = f(Fix(n2))
          val f2: F[R => Attr[F, R]] = ff.map(_ compose r)
          Attr(r1, f2.map(_(r1)))
        }
      })
      .apply(n)
      .apply(root)

  /** Annotate (top-down) each node with an Int, computed as +1 the parent's annotation. */
  def withDepth[F[_]: Functor](fixed: Fix[F]): Attr[F, Int] =
    inherit(Function.const((_: Int) + 1), 0, fixed)

  def prettyAlg2[V: Show](indent: Int = 2) =
    Algebra[AttrF[RoseF[V, ?], Int, ?], String] {
      case AttrF(depth, RoseF(root, children)) =>
        val prefix = (" " * indent) * depth
        if (children.isEmpty) show"$prefix$root"
        else
          show"$prefix($root${children.mkString_("\n", "\n", "")})"
    }

  def pretty2[V: Show](f: Fixed[V], indent: Int = 2): String =
    scheme
      .hylo(prettyAlg2[V](indent), Attr.coalgebra[RoseF[V, ?], Int])
      .apply(withDepth(f))

  def pretty3[F[_]: Functor](f: Fix[F], indent: Int = 2): String =
    scheme
      .hylo(
        Algebra[AttrF[F, Int, ?], String] {
          case AttrF(depth, v) =>
            val prefix = (" " * indent) * depth
            prefix + v.toString + "\n"
        },
        Attr.coalgebra[F, Int]
      )
      .apply(withDepth[F](f))
}

object TermFExample extends App {
  val t0: TermF.Fixed =
    TermF
      .And(
        TermF.True().fix[TermF],
        TermF
          .Or(
            TermF.False().fix[TermF],
            TermF
              .Implies(TermF.Type("A").fix[TermF], TermF.Type("B").fix[TermF])
              .fix[TermF]
          )
          .fix[TermF]
      )
      .fix[TermF]
  val tree0 = TermF.toTree(t0)
  val tree1 = RoseF.h(tree0).runA(0).value
  println(RoseF.print(tree0))
  println(RoseF.print(tree1))
  println(RoseF.pretty(tree0))

  println()

  val tree2 = RoseF.withDepth(tree0)
  println(tree2)
  println(RoseF.pretty2(tree0))
  println()
  println(RoseF.pretty3[TermF](t0))
}
