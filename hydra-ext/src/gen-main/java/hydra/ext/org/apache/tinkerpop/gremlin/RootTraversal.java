// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public class RootTraversal implements Serializable, Comparable<RootTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal");
  
  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name SPAWN_METHOD = new hydra.core.Name("spawnMethod");
  
  public static final hydra.core.Name CHAINED = new hydra.core.Name("chained");
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource source;
  
  public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod;
  
  public final hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement> chained;
  
  public RootTraversal (hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource source, hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod, hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement> chained) {
    this.source = source;
    this.spawnMethod = spawnMethod;
    this.chained = chained;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RootTraversal)) {
      return false;
    }
    RootTraversal o = (RootTraversal) other;
    return java.util.Objects.equals(
      this.source,
      o.source) && java.util.Objects.equals(
      this.spawnMethod,
      o.spawnMethod) && java.util.Objects.equals(
      this.chained,
      o.chained);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(source) + 3 * java.util.Objects.hashCode(spawnMethod) + 5 * java.util.Objects.hashCode(chained);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RootTraversal other) {
    int cmp = 0;
    cmp = ((Comparable) source).compareTo(other.source);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) spawnMethod).compareTo(other.spawnMethod);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      chained.hashCode(),
      other.chained.hashCode());
  }
  
  public RootTraversal withSource(hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource source) {
    return new RootTraversal(source, spawnMethod, chained);
  }
  
  public RootTraversal withSpawnMethod(hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod) {
    return new RootTraversal(source, spawnMethod, chained);
  }
  
  public RootTraversal withChained(hydra.util.ConsList<hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement> chained) {
    return new RootTraversal(source, spawnMethod, chained);
  }
}
