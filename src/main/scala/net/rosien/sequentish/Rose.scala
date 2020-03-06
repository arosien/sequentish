package net.rosien.sequentish

import cats._
import cats.implicits._

case class Rose[A](root: A, children: List[Rose[A]])

object Rose {

  def unfold[A](root: A)(f: A => List[A]): Rose[A] =
    Rose(root, f(root).map(unfold(_)(f)))

  implicit val functor: Functor[Rose] = new Functor[Rose] {
    def map[A, B](fa: Rose[A])(f: A => B): Rose[B] =
      Rose(f(fa.root), fa.children.map(_ map f))
  }

  implicit def show[A: Show]: Show[Rose[A]] = tree => {
    def c(level: Int, children: List[Rose[A]]): String =
      if (children.isEmpty) ""
      else
        children
          .map(s(level + 1))
          .mkString("\n", "\n", "")
    def s(level: Int)(t: Rose[A]): String = t match {
      case Rose(root, children) =>
        show"${" " * level}($root${c(level, children)})"
    }
    s(0)(tree)
  }
}
