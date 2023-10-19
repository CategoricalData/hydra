package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public abstract class BadVertex<T, V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.BadVertex");
  
  private BadVertex () {
  
  }
  
  public abstract <R> R accept(Visitor<T, V, R> visitor) ;
  
  public interface Visitor<T, V, R> {
    R visit(Label<T, V> instance) ;
    
    R visit(LabelUnexpected<T, V> instance) ;
    
    R visit(Id<T, V> instance) ;
    
    R visit(Property<T, V> instance) ;
  }
  
  public interface PartialVisitor<T, V, R> extends Visitor<T, V, R> {
    default R otherwise(BadVertex<T, V> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Label<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(LabelUnexpected<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(Id<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(Property<T, V> instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A vertex label mismatch
   */
  public static final class Label<T, V> extends hydra.langs.tinkerpop.errors.BadVertex<T, V> implements Serializable {
    /**
     * A vertex label mismatch
     */
    public final hydra.langs.tinkerpop.errors.VertexLabelMismatch value;
    
    public Label (hydra.langs.tinkerpop.errors.VertexLabelMismatch value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Label)) {
        return false;
      }
      Label o = (Label) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * The label of the vertex does not have an associated vertex type
   */
  public static final class LabelUnexpected<T, V> extends hydra.langs.tinkerpop.errors.BadVertex<T, V> implements Serializable {
    /**
     * The label of the vertex does not have an associated vertex type
     */
    public final hydra.langs.tinkerpop.propertyGraph.VertexLabel value;
    
    public LabelUnexpected (hydra.langs.tinkerpop.propertyGraph.VertexLabel value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LabelUnexpected)) {
        return false;
      }
      LabelUnexpected o = (LabelUnexpected) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A vertex id type error
   */
  public static final class Id<T, V> extends hydra.langs.tinkerpop.errors.BadVertex<T, V> implements Serializable {
    /**
     * A vertex id type error
     */
    public final hydra.langs.tinkerpop.errors.TypeError<T, V> value;
    
    public Id (hydra.langs.tinkerpop.errors.TypeError<T, V> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Id)) {
        return false;
      }
      Id o = (Id) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A vertex property error
   */
  public static final class Property<T, V> extends hydra.langs.tinkerpop.errors.BadVertex<T, V> implements Serializable {
    /**
     * A vertex property error
     */
    public final hydra.langs.tinkerpop.errors.BadProperty<T, V> value;
    
    public Property (hydra.langs.tinkerpop.errors.BadProperty<T, V> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<T, V, R> visitor) {
      return visitor.visit(this);
    }
  }
}