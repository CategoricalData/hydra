// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The direction of an edge or edge pattern
 */
public abstract class Direction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.Direction");
  
  public static final hydra.core.Name FIELD_NAME_OUT = new hydra.core.Name("out");
  
  public static final hydra.core.Name FIELD_NAME_IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name FIELD_NAME_BOTH = new hydra.core.Name("both");
  
  public static final hydra.core.Name FIELD_NAME_UNDIRECTED = new hydra.core.Name("undirected");
  
  private Direction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Out instance) ;
    
    R visit(In instance) ;
    
    R visit(Both instance) ;
    
    R visit(Undirected instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Direction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Out instance) {
      return otherwise((instance));
    }
    
    default R visit(In instance) {
      return otherwise((instance));
    }
    
    default R visit(Both instance) {
      return otherwise((instance));
    }
    
    default R visit(Undirected instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Out extends hydra.pg.model.Direction implements Serializable {
    public final Boolean value;
    
    public Out (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof Out;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class In extends hydra.pg.model.Direction implements Serializable {
    public final Boolean value;
    
    public In (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof In;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Both extends hydra.pg.model.Direction implements Serializable {
    public final Boolean value;
    
    public Both (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof Both;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Undirected extends hydra.pg.model.Direction implements Serializable {
    public final Boolean value;
    
    public Undirected (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof Undirected;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
