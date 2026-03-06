// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class Projections implements Serializable, Comparable<Projections> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.Projections");
  
  public static final hydra.core.Name ALL = new hydra.core.Name("all");
  
  public static final hydra.core.Name EXPLICIT = new hydra.core.Name("explicit");
  
  public final Boolean all;
  
  public final java.util.List<hydra.pg.query.Projection> explicit;
  
  public Projections (Boolean all, java.util.List<hydra.pg.query.Projection> explicit) {
    this.all = all;
    this.explicit = explicit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projections)) {
      return false;
    }
    Projections o = (Projections) other;
    return java.util.Objects.equals(
      this.all,
      o.all) && java.util.Objects.equals(
      this.explicit,
      o.explicit);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(all) + 3 * java.util.Objects.hashCode(explicit);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Projections other) {
    int cmp = 0;
    cmp = ((Comparable) all).compareTo(other.all);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      explicit.hashCode(),
      other.explicit.hashCode());
  }
  
  public Projections withAll(Boolean all) {
    return new Projections(all, explicit);
  }
  
  public Projections withExplicit(java.util.List<hydra.pg.query.Projection> explicit) {
    return new Projections(all, explicit);
  }
}
