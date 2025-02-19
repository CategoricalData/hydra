// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Template implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Template");
  
  public static final hydra.core.Name FIELD_NAME_EARLY = new hydra.core.Name("early");
  
  public static final hydra.core.Name FIELD_NAME_INITS = new hydra.core.Name("inits");
  
  public static final hydra.core.Name FIELD_NAME_SELF = new hydra.core.Name("self");
  
  public static final hydra.core.Name FIELD_NAME_STATS = new hydra.core.Name("stats");
  
  public final java.util.List<hydra.ext.scala.meta.Stat> early;
  
  public final java.util.List<hydra.ext.scala.meta.Init> inits;
  
  public final hydra.ext.scala.meta.Self self;
  
  public final java.util.List<hydra.ext.scala.meta.Stat> stats;
  
  public Template (java.util.List<hydra.ext.scala.meta.Stat> early, java.util.List<hydra.ext.scala.meta.Init> inits, hydra.ext.scala.meta.Self self, java.util.List<hydra.ext.scala.meta.Stat> stats) {
    java.util.Objects.requireNonNull((early));
    java.util.Objects.requireNonNull((inits));
    java.util.Objects.requireNonNull((self));
    java.util.Objects.requireNonNull((stats));
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
    Template o = (Template) (other);
    return early.equals(o.early) && inits.equals(o.inits) && self.equals(o.self) && stats.equals(o.stats);
  }
  
  @Override
  public int hashCode() {
    return 2 * early.hashCode() + 3 * inits.hashCode() + 5 * self.hashCode() + 7 * stats.hashCode();
  }
  
  public Template withEarly(java.util.List<hydra.ext.scala.meta.Stat> early) {
    java.util.Objects.requireNonNull((early));
    return new Template(early, inits, self, stats);
  }
  
  public Template withInits(java.util.List<hydra.ext.scala.meta.Init> inits) {
    java.util.Objects.requireNonNull((inits));
    return new Template(early, inits, self, stats);
  }
  
  public Template withSelf(hydra.ext.scala.meta.Self self) {
    java.util.Objects.requireNonNull((self));
    return new Template(early, inits, self, stats);
  }
  
  public Template withStats(java.util.List<hydra.ext.scala.meta.Stat> stats) {
    java.util.Objects.requireNonNull((stats));
    return new Template(early, inits, self, stats);
  }
}