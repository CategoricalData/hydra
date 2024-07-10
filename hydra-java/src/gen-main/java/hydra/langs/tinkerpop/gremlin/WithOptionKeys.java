// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class WithOptionKeys implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.WithOptionKeys");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ShortestPath instance) {
      return otherwise((instance));
    }
    
    default R visit(ConnectedComponent instance) {
      return otherwise((instance));
    }
    
    default R visit(PageRank instance) {
      return otherwise((instance));
    }
    
    default R visit(PeerPressure instance) {
      return otherwise((instance));
    }
    
    default R visit(Io instance) {
      return otherwise((instance));
    }
    
    default R visit(WithOptionsTokens instance) {
      return otherwise((instance));
    }
    
    default R visit(WithOptionsIndexer instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ShortestPath extends hydra.langs.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ShortestPathConstants value;
    
    public ShortestPath (hydra.langs.tinkerpop.gremlin.ShortestPathConstants value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShortestPath)) {
        return false;
      }
      ShortestPath o = (ShortestPath) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ConnectedComponent extends hydra.langs.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ConnectedComponentConstants value;
    
    public ConnectedComponent (hydra.langs.tinkerpop.gremlin.ConnectedComponentConstants value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConnectedComponent)) {
        return false;
      }
      ConnectedComponent o = (ConnectedComponent) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PageRank extends hydra.langs.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.PageRankConstants value;
    
    public PageRank (hydra.langs.tinkerpop.gremlin.PageRankConstants value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PageRank)) {
        return false;
      }
      PageRank o = (PageRank) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PeerPressure extends hydra.langs.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.PeerPressureConstants value;
    
    public PeerPressure (hydra.langs.tinkerpop.gremlin.PeerPressureConstants value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PeerPressure)) {
        return false;
      }
      PeerPressure o = (PeerPressure) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Io extends hydra.langs.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.IoOptionsKeys value;
    
    public Io (hydra.langs.tinkerpop.gremlin.IoOptionsKeys value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Io)) {
        return false;
      }
      Io o = (Io) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class WithOptionsTokens extends hydra.langs.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public WithOptionsTokens () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithOptionsTokens)) {
        return false;
      }
      WithOptionsTokens o = (WithOptionsTokens) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class WithOptionsIndexer extends hydra.langs.tinkerpop.gremlin.WithOptionKeys implements Serializable {
    public WithOptionsIndexer () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WithOptionsIndexer)) {
        return false;
      }
      WithOptionsIndexer o = (WithOptionsIndexer) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}