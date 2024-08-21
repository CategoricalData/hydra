// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class RootTraversal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/gremlin.RootTraversal");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_SPAWN_METHOD = new hydra.core.Name("spawnMethod");
  
  public static final hydra.core.Name FIELD_NAME_CHAINED = new hydra.core.Name("chained");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource source;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod;
  
  public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement> chained;
  
  public RootTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource source, hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod, java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement> chained) {
    java.util.Objects.requireNonNull((source));
    java.util.Objects.requireNonNull((spawnMethod));
    java.util.Objects.requireNonNull((chained));
    this.source = source;
    this.spawnMethod = spawnMethod;
    this.chained = chained;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RootTraversal)) {
      return false;
    }
    RootTraversal o = (RootTraversal) (other);
    return source.equals(o.source) && spawnMethod.equals(o.spawnMethod) && chained.equals(o.chained);
  }
  
  @Override
  public int hashCode() {
    return 2 * source.hashCode() + 3 * spawnMethod.hashCode() + 5 * chained.hashCode();
  }
  
  public RootTraversal withSource(hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource source) {
    java.util.Objects.requireNonNull((source));
    return new RootTraversal(source, spawnMethod, chained);
  }
  
  public RootTraversal withSpawnMethod(hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod) {
    java.util.Objects.requireNonNull((spawnMethod));
    return new RootTraversal(source, spawnMethod, chained);
  }
  
  public RootTraversal withChained(java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement> chained) {
    java.util.Objects.requireNonNull((chained));
    return new RootTraversal(source, spawnMethod, chained);
  }
}