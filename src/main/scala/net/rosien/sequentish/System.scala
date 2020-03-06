package net.rosien.sequentish

import cats.data.NonEmptySet

// tag::system[]
/** A [[System]] contains a set of rules. */
case class System[Rule](rules: NonEmptySet[Rule])
// end::system[]
