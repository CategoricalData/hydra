// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class OptionArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.OptionArgs");
  
  private OptionArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(PredicateTraversal instance) ;
    
    R visit(MergeMap instance) ;
    
    R visit(MergeTraversal instance) ;
    
    R visit(ObjectTraversal instance) ;
    
    R visit(Traversal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OptionArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(PredicateTraversal instance) {
      return otherwise((instance));
    }
    
    default R visit(MergeMap instance) {
      return otherwise((instance));
    }
    
    default R visit(MergeTraversal instance) {
      return otherwise((instance));
    }
    
    default R visit(ObjectTraversal instance) {
      return otherwise((instance));
    }
    
    default R visit(Traversal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class PredicateTraversal extends hydra.langs.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal value;
    
    public PredicateTraversal (hydra.langs.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PredicateTraversal)) {
        return false;
      }
      PredicateTraversal o = (PredicateTraversal) (other);
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
  
  public static final class MergeMap extends hydra.langs.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument value;
    
    public MergeMap (hydra.langs.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeMap)) {
        return false;
      }
      MergeMap o = (MergeMap) (other);
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
  
  public static final class MergeTraversal extends hydra.langs.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal value;
    
    public MergeTraversal (hydra.langs.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeTraversal)) {
        return false;
      }
      MergeTraversal o = (MergeTraversal) (other);
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
  
  public static final class ObjectTraversal extends hydra.langs.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal value;
    
    public ObjectTraversal (hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectTraversal)) {
        return false;
      }
      ObjectTraversal o = (ObjectTraversal) (other);
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
  
  public static final class Traversal extends hydra.langs.tinkerpop.gremlin.OptionArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Traversal (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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