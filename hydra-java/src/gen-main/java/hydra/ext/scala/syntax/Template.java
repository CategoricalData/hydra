// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Template implements Serializable, Comparable<Template> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Template");

  public static final hydra.core.Name EARLY = new hydra.core.Name("early");

  public static final hydra.core.Name INITS = new hydra.core.Name("inits");

  public static final hydra.core.Name SELF = new hydra.core.Name("self");

  public static final hydra.core.Name STATS = new hydra.core.Name("stats");

  public final java.util.List<hydra.ext.scala.syntax.Stat> early;

  public final java.util.List<hydra.ext.scala.syntax.Init> inits;

  public final hydra.ext.scala.syntax.Self self;

  public final java.util.List<hydra.ext.scala.syntax.Stat> stats;

  public Template (java.util.List<hydra.ext.scala.syntax.Stat> early, java.util.List<hydra.ext.scala.syntax.Init> inits, hydra.ext.scala.syntax.Self self, java.util.List<hydra.ext.scala.syntax.Stat> stats) {
    this.early = early;
    this.inits = inits;
    this.self = self;
    this.stats = stats;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Template)) {
      return false;
    }
    Template o = (Template) other;
    return java.util.Objects.equals(
      this.early,
      o.early) && java.util.Objects.equals(
      this.inits,
      o.inits) && java.util.Objects.equals(
      this.self,
      o.self) && java.util.Objects.equals(
      this.stats,
      o.stats);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(early) + 3 * java.util.Objects.hashCode(inits) + 5 * java.util.Objects.hashCode(self) + 7 * java.util.Objects.hashCode(stats);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Template other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      early,
      other.early);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      inits,
      other.inits);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      self,
      other.self);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      stats,
      other.stats);
  }

  public Template withEarly(java.util.List<hydra.ext.scala.syntax.Stat> early) {
    return new Template(early, inits, self, stats);
  }

  public Template withInits(java.util.List<hydra.ext.scala.syntax.Init> inits) {
    return new Template(early, inits, self, stats);
  }

  public Template withSelf(hydra.ext.scala.syntax.Self self) {
    return new Template(early, inits, self, stats);
  }

  public Template withStats(java.util.List<hydra.ext.scala.syntax.Stat> stats) {
    return new Template(early, inits, self, stats);
  }
}
