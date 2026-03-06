// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WithOptionKeys implements Serializable, Comparable<WithOptionKeys> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys");
  
  public static final hydra.core.Name SHORTEST_PATH = new hydra.core.Name("shortestPath");
  
  public static final hydra.core.Name CONNECTED_COMPONENT = new hydra.core.Name("connectedComponent");
  
  public static final hydra.core.Name PAGE_RANK = new hydra.core.Name("pageRank");
  
  public static final hydra.core.Name PEER_PRESSURE = new hydra.core.Name("peerPressure");
  
  public static final hydra.core.Name IO = new hydra.core.Name("io");
  
  public static final hydra.core.Name WITH_OPTIONS_TOKENS = new hydra.core.Name("withOptionsTokens");
  
  public static final hydra.core.Name WITH_OPTIONS_INDEXER = new hydra.core.Name("withOptionsIndexer");
  
  private WithOptionKeys () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ShortestPath instance) ;
    
    R visit(ConnectedComponent instance) ;
    
    R visit(PageRank instance) ;
    
    R visit(PeerPressure instance) ;
    
    R visit(Io instance) ;
    
    R visit(WithOptionsTokens instance) ;
    
    R visit(WithOptionsIndexer instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WithOptionKeys instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(ShortestPath instance) {
      return otherwise(instance);
    }
    
    default R visit(ConnectedComponent instance) {
      return otherwise(instance);
    }
    
    default R visit(PageRank instance) {
      return otherwise(instance);
    }
    
    default R visit(PeerPressure instance) {
      return otherwise(instance);
    }
    
    default R visit(Io instance) {
      return otherwise(instance);
    }
    
    default R visit(WithOptionsTokens instance) {
      return otherwise(instance);
    }
    
    default R visit(WithOptionsIndexer instance) {
      return otherwise(instance);
    }
  }
  
  public static final class ShortestPath extends hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants value;
    
    public ShortestPath (hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShortestPath)) {
        return false;
      }
      ShortestPath o = (ShortestPath) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(WithOptionKeys other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ShortestPath o = (ShortestPath) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ConnectedComponent extends hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ConnectedComponentConstants value;
    
    public ConnectedComponent (hydra.ext.org.apache.tinkerpop.gremlin.ConnectedComponentConstants value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConnectedComponent)) {
        return false;
      }
      ConnectedComponent o = (ConnectedComponent) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(WithOptionKeys other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ConnectedComponent o = (ConnectedComponent) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PageRank extends hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.PageRankConstants value;
    
    public PageRank (hydra.ext.org.apache.tinkerpop.gremlin.PageRankConstants value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PageRank)) {
        return false;
      }
      PageRank o = (PageRank) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(WithOptionKeys other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PageRank o = (PageRank) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PeerPressure extends hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.PeerPressureConstants value;
    
    public PeerPressure (hydra.ext.org.apache.tinkerpop.gremlin.PeerPressureConstants value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PeerPressure)) {
        return false;
      }
      PeerPressure o = (PeerPressure) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(WithOptionKeys other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PeerPressure o = (PeerPressure) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Io extends hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsKeys value;
    
    public Io (hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsKeys value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Io)) {
        return false;
      }
      Io o = (Io) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(WithOptionKeys other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Io o = (Io) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class WithOptionsTokens extends hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public WithOptionsTokens () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithOptionsTokens)) {
        return false;
      }
      WithOptionsTokens o = (WithOptionsTokens) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(WithOptionKeys other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class WithOptionsIndexer extends hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public WithOptionsIndexer () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithOptionsIndexer)) {
        return false;
      }
      WithOptionsIndexer o = (WithOptionsIndexer) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(WithOptionKeys other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
