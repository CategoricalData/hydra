// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public class RootTraversal implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.RootTraversal");
  
  public final hydra.langs.tinkerpop.gremlin.TraversalSource source;
  
  public final hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod;
  
  public final java.util.List<hydra.langs.tinkerpop.gremlin.ChainedTraversalElement> chained;
  
  public RootTraversal (hydra.langs.tinkerpop.gremlin.TraversalSource source, hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod, java.util.List<hydra.langs.tinkerpop.gremlin.ChainedTraversalElement> chained) {
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    if (spawnMethod == null) {
      throw new IllegalArgumentException("null value for 'spawnMethod' argument");
    }
    if (chained == null) {
      throw new IllegalArgumentException("null value for 'chained' argument");
    }
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
  
  public RootTraversal withSource(hydra.langs.tinkerpop.gremlin.TraversalSource source) {
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    return new RootTraversal(source, spawnMethod, chained);
  }
  
  public RootTraversal withSpawnMethod(hydra.langs.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod) {
    if (spawnMethod == null) {
      throw new IllegalArgumentException("null value for 'spawnMethod' argument");
    }
    return new RootTraversal(source, spawnMethod, chained);
  }
  
  public RootTraversal withChained(java.util.List<hydra.langs.tinkerpop.gremlin.ChainedTraversalElement> chained) {
    if (chained == null) {
      throw new IllegalArgumentException("null value for 'chained' argument");
    }
    return new RootTraversal(source, spawnMethod, chained);
  }
}