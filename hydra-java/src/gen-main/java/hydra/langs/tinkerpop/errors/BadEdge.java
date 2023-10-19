package hydra.langs.tinkerpop.errors;

import java.io.Serializable;

public abstract class BadEdge<T, V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/errors.BadEdge");
  
  private BadEdge () {
  
  }
  
  public abstract <R> R accept(Visitor<T, V, R> visitor) ;
  
  public interface Visitor<T, V, R> {
    R visit(Label<T, V> instance) ;
    
    R visit(LabelUnexpected<T, V> instance) ;
    
    R visit(Id<T, V> instance) ;
    
    R visit(Property<T, V> instance) ;
    
    R visit(NoSuchOutVertex<T, V> instance) ;
    
    R visit(NoSuchInVertex<T, V> instance) ;
    
    R visit(WrongOutVertexLabel<T, V> instance) ;
    
    R visit(WrongInVertexLabel<T, V> instance) ;
  }
  
  public interface PartialVisitor<T, V, R> extends Visitor<T, V, R> {
    default R otherwise(BadEdge<T, V> instance) {
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
    
    default R visit(NoSuchOutVertex<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(NoSuchInVertex<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(WrongOutVertexLabel<T, V> instance) {
      return otherwise((instance));
    }
    
    default R visit(WrongInVertexLabel<T, V> instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * An edge label mismatch
   */
  public static final class Label<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * An edge label mismatch
     */
    public final hydra.langs.tinkerpop.errors.EdgeLabelMismatch value;
    
    public Label (hydra.langs.tinkerpop.errors.EdgeLabelMismatch value) {
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
   * The label of the edge does not have an associated edge type
   */
  public static final class LabelUnexpected<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * The label of the edge does not have an associated edge type
     */
    public final hydra.langs.tinkerpop.propertyGraph.EdgeLabel value;
    
    public LabelUnexpected (hydra.langs.tinkerpop.propertyGraph.EdgeLabel value) {
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
   * An edge id type error
   */
  public static final class Id<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * An edge id type error
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
   * An edge property error
   */
  public static final class Property<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * An edge property error
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
  
  /**
   * The out-vertex of the edge does not exist
   */
  public static final class NoSuchOutVertex<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * The out-vertex of the edge does not exist
     */
    public final V value;
    
    public NoSuchOutVertex (V value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoSuchOutVertex)) {
        return false;
      }
      NoSuchOutVertex o = (NoSuchOutVertex) (other);
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
   * The in-vertex of the edge does not exist
   */
  public static final class NoSuchInVertex<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * The in-vertex of the edge does not exist
     */
    public final V value;
    
    public NoSuchInVertex (V value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoSuchInVertex)) {
        return false;
      }
      NoSuchInVertex o = (NoSuchInVertex) (other);
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
   * The out-vertex of the edge has the wrong label
   */
  public static final class WrongOutVertexLabel<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * The out-vertex of the edge has the wrong label
     */
    public final hydra.langs.tinkerpop.errors.VertexLabelMismatch value;
    
    public WrongOutVertexLabel (hydra.langs.tinkerpop.errors.VertexLabelMismatch value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WrongOutVertexLabel)) {
        return false;
      }
      WrongOutVertexLabel o = (WrongOutVertexLabel) (other);
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
   * The in-vertex of the edge has the wrong label
   */
  public static final class WrongInVertexLabel<T, V> extends hydra.langs.tinkerpop.errors.BadEdge<T, V> implements Serializable {
    /**
     * The in-vertex of the edge has the wrong label
     */
    public final hydra.langs.tinkerpop.errors.VertexLabelMismatch value;
    
    public WrongInVertexLabel (hydra.langs.tinkerpop.errors.VertexLabelMismatch value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof WrongInVertexLabel)) {
        return false;
      }
      WrongInVertexLabel o = (WrongInVertexLabel) (other);
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