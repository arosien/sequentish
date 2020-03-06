package net.rosien.sequentish

import cats.implicits._

/**
  * <pre>
  * ( ⊢ (((A ⇒ A) ⇒ B) ⇒ B)
  *  (R⇒
  *   (((A ⇒ A) ⇒ B) ⊢ B
  *    (L⇒4
  *     ((A ⇒ B) ⊢ (A ⇒ A)
  *      (R⇒
  *       (A, (A ⇒ B) ⊢ A
  *        (Id))))
  *     (B ⊢ B
  *      (Id))))))
  * </pre>
  */
trait Interpreter[Rule] {
  def run[A](proof: Proof[Rule]): A
}

case class P2[Rule](sequent: Sequent, rule: Rule, proofs: List[P2[Rule]])

object P2 {
  def fromProof[Rule](proof: Proof[Rule]): Option[P2[Rule]] = {
    def fromProof2(sequent: Sequent, rp: RuleProof[Rule]): Option[P2[Rule]] =
      rp match {
        case RuleProof.Stuck(_)         => None
        case RuleProof.Discharged(rule) => Some(P2(sequent, rule, Nil))
        case RuleProof.And(rule, proofs) =>
          proofs.traverse(fromProof).map(and => P2(sequent, rule, and.toList))
      }

    proof match {
      case Proof.Root(sequent, any) =>
        any.map(fromProof2(sequent, _)).find(_.isDefined).flatten
    }
  }
}
