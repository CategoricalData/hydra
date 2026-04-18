// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class RootTraversal implements Serializable, Comparable<RootTraversal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.RootTraversal");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name SPAWN_METHOD = new hydra.core.Name("spawnMethod");

  public static final hydra.core.Name CHAINED = new hydra.core.Name("chained");

  public final hydra.tinkerpop.gremlin.TraversalSource source;

  public final hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod;

  public final java.util.List<hydra.tinkerpop.gremlin.ChainedTraversalElement> chained;

  public RootTraversal (hydra.tinkerpop.gremlin.TraversalSource source, hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod, java.util.List<hydra.tinkerpop.gremlin.ChainedTraversalElement> chained) {
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
    cmp = hydra.util.Comparing.compare(
      source,
      other.source);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      spawnMethod,
      other.spawnMethod);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      chained,
      other.chained);
  }

  public RootTraversal withSource(hydra.tinkerpop.gremlin.TraversalSource source) {
    return new RootTraversal(source, spawnMethod, chained);
  }

  public RootTraversal withSpawnMethod(hydra.tinkerpop.gremlin.TraversalSourceSpawnMethod spawnMethod) {
    return new RootTraversal(source, spawnMethod, chained);
  }

  public RootTraversal withChained(java.util.List<hydra.tinkerpop.gremlin.ChainedTraversalElement> chained) {
    return new RootTraversal(source, spawnMethod, chained);
  }
}
