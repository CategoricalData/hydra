package hydra.overlay.scala.lib

object equality:
  // Delegates to ordering.compareTerms (hydra.overlay.scala.lib.ordering) rather than a
  // separate implementation. The previous version special-cased a BARE top-level BigDecimal
  // (scala.math.BigDecimal's own equals/hashCode are scale-BLIND by design, delegating to
  // compareTo, unlike java.math.BigDecimal) but fell to native `==` for everything else --
  // correct for a decimal at the term's own top level, but WRONG for one nested inside a
  // Map/Set/Record/etc.: native `==` recurses structurally into case classes/collections,
  // but that recursion eventually reaches a nested BigDecimal's own scale-blind equals,
  // silently losing scale-distinctness (docs/specification/ordering-and-equality.md: 1.1 !=
  // 1.10) below the top level (#742). compareTerms's BigDecimal branch fires at every
  // recursion depth already (it recurses via itself, not a shallow pre-check), so delegating
  // to it fixes decimal equality at any nesting depth, not just the top level.
  def equal[A](x: A)(y: A): Boolean = ordering.compareTerms(x, y) == 0
