package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class Projections implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.Projections");
  
  public final Boolean all;
  
  public final java.util.List<hydra.langs.tinkerpop.queries.Projection> explicit;
  
  public Projections (Boolean all, java.util.List<hydra.langs.tinkerpop.queries.Projection> explicit) {
    this.all = all;
    this.explicit = explicit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Projections)) {
      return false;
    }
    Projections o = (Projections) (other);
    return all.equals(o.all) && explicit.equals(o.explicit);
  }
  
  @Override
  public int hashCode() {
    return 2 * all.hashCode() + 3 * explicit.hashCode();
  }
  
  public Projections withAll(Boolean all) {
    return new Projections(all, explicit);
  }
  
  public Projections withExplicit(java.util.List<hydra.langs.tinkerpop.queries.Projection> explicit) {
    return new Projections(all, explicit);
  }
}