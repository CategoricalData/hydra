package hydra.ext.scala.meta;

public class Template {
  public final java.util.List<Stat> early;
  
  public final java.util.List<Init> inits;
  
  public final Self self;
  
  public final java.util.List<Stat> stats;
  
  public Template (java.util.List<Stat> early, java.util.List<Init> inits, Self self, java.util.List<Stat> stats) {
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
  
  public Template withEarly(java.util.List<Stat> early) {
    return new Template(early, inits, self, stats);
  }
  
  public Template withInits(java.util.List<Init> inits) {
    return new Template(early, inits, self, stats);
  }
  
  public Template withSelf(Self self) {
    return new Template(early, inits, self, stats);
  }
  
  public Template withStats(java.util.List<Stat> stats) {
    return new Template(early, inits, self, stats);
  }
}