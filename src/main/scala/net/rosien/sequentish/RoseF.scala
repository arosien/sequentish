package net.rosien.sequentish

import cats._
import cats.implicits._
import higherkindness.droste._
import higherkindness.droste.data._
import higherkindness.droste.data.prelude._
import higherkindness.droste.util.DefaultTraverse

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

  def print[V: Show](f: Fixed[V]): String =
    scheme
      .cata(Algebra[RoseF[V, ?], String] {
        case RoseF(root, children) =>
          if (children.isEmpty) root.show
          else show"($root, (${children.mkString(", ")}))"
      })
      .apply(f)

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

  type Pretty[F[_], A] = AttrF[F, Int, A]
  type PrettyAlg[F[_]] = Algebra[Pretty[F, ?], String]

  def prettyAlg[V: Show](indent: Int = 2): PrettyAlg[RoseF[V, ?]] =
    Algebra[AttrF[RoseF[V, ?], Int, ?], String] {
      case AttrF(depth, RoseF(root, children)) =>
        val prefix = (" " * indent) * depth
        if (children.isEmpty) show"$prefix$root"
        else
          show"$prefix($root${children.mkString_("\n", "\n", "")})"
    }

  def pretty[F[_]: Functor](alg: PrettyAlg[F]): Fix[F] => String =
    f => scheme
      .hylo(alg, Attr.coalgebra[F, Int])
      .apply(withDepth(f))

  def pretty2[V: Show](indent: Int = 2): Fixed[V] => String =
    pretty[RoseF[V, ?]](prettyAlg[V](indent))
}
