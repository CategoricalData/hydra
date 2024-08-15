// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ToArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ToArgs");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTION = new hydra.core.Name("direction");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_VERTEX = new hydra.core.Name("vertex");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL = new hydra.core.Name("traversal");
  
  private ToArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Direction instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Vertex instance) ;
    
    R visit(Traversal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ToArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Direction instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Vertex instance) {
      return otherwise((instance));
    }
    
    default R visit(Traversal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Direction extends hydra.langs.tinkerpop.gremlin.ToArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.DirectionAndVarargs value;
    
    public Direction (hydra.langs.tinkerpop.gremlin.DirectionAndVarargs value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Direction)) {
        return false;
      }
      Direction o = (Direction) (other);
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
  
  public static final class String_ extends hydra.langs.tinkerpop.gremlin.ToArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public String_ (hydra.langs.tinkerpop.gremlin.StringArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class Vertex extends hydra.langs.tinkerpop.gremlin.ToArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StructureVertexArgument value;
    
    public Vertex (hydra.langs.tinkerpop.gremlin.StructureVertexArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) (other);
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
  
  public static final class Traversal extends hydra.langs.tinkerpop.gremlin.ToArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Traversal (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Traversal)) {
        return false;
      }
      Traversal o = (Traversal) (other);
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
}