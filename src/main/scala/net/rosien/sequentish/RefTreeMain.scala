package net.rosien.sequentish


object RefTreeMain {

  // val proof2 = Prover[LJT].prove2(s0).prune

  // import reftree.core._
  // import reftree.render.{Renderer, RenderingOptions}
  // import reftree.diagram.Diagram
  // import java.nio.file.Paths

  // implicit val s: ToRefTree[Sequent] = ToRefTree(s => RefTree.Ref(show"${s.premises.mkString_(", ")} ⊢ ${s.conclusion}", s.hashCode.abs.toString, Seq.empty, true))
  // implicit val r: ToRefTree[LJT] = ToRefTree(s => RefTree.Ref(s.show, s.hashCode.toString, Seq.empty, true))
  // implicit val t: ToRefTree[Term] = ToRefTree(s => RefTree.Ref(s.show, s.hashCode.toString, Seq.empty, true))

  // val renderer = Renderer(
  //   renderingOptions = RenderingOptions(density = 75),
  //   directory = Paths.get("target")
  // )
  // import renderer._

  // Diagram(proof2).render("proof2")

}