// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class HasTraversalTokenArgsRest implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/gremlin.HasTraversalTokenArgsRest");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL = new hydra.core.Name("traversal");
  
  private HasTraversalTokenArgsRest () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(Predicate instance) ;
    
    R visit(Traversal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(HasTraversalTokenArgsRest instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Predicate instance) {
      return otherwise((instance));
    }
    
    default R visit(Traversal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Literal (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  public static final class Predicate extends hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value;
    
    public Predicate (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predicate)) {
        return false;
      }
      Predicate o = (Predicate) (other);
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
  
  public static final class Traversal extends hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public Traversal (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
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