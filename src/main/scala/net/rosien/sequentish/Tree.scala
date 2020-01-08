package net.rosien.sequentish

import cats._
import cats.implicits._

case class Tree[A](root: A, children: List[Tree[A]])

object Tree {

  def unfold[A](root: A)(f: A => List[A]): Tree[A] =
    Tree(root, f(root).map(unfold(_)(f)))

  implicit val functor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.root), fa.children.map(_ map f))
  }

  implicit def show[A: Show]: Show[Tree[A]] = tree => {
    def c(level: Int, children: List[Tree[A]]): String =
      if (children.isEmpty) ""
      else
        children
          .map(s(level + 1))
          .mkString("\n", "\n", "")
    def s(level: Int)(t: Tree[A]): String = t match {
      case Tree(root, children) =>
        show"${" " * level}($root${c(level, children)})"
    }
    s(0)(tree)
  }
}
