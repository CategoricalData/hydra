// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.queries;

import java.io.Serializable;

public class Projections implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/queries.Projections");
  
  public static final hydra.core.Name FIELD_NAME_ALL = new hydra.core.Name("all");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICIT = new hydra.core.Name("explicit");
  
  public final Boolean all;
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.queries.Projection> explicit;
  
  public Projections (Boolean all, java.util.List<hydra.ext.org.apache.tinkerpop.queries.Projection> explicit) {
    java.util.Objects.requireNonNull((all));
    java.util.Objects.requireNonNull((explicit));
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
    java.util.Objects.requireNonNull((all));
    return new Projections(all, explicit);
  }
  
  public Projections withExplicit(java.util.List<hydra.ext.org.apache.tinkerpop.queries.Projection> explicit) {
    java.util.Objects.requireNonNull((explicit));
    return new Projections(all, explicit);
  }
}