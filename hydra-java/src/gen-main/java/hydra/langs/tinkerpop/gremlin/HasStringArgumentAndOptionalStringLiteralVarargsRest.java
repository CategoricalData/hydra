// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class HasStringArgumentAndOptionalStringLiteralVarargsRest implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest");
  
  private HasStringArgumentAndOptionalStringLiteralVarargsRest () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Object_ instance) ;
    
    R visit(Predicate instance) ;
    
    R visit(StringObject instance) ;
    
    R visit(StringPredicate instance) ;
    
    R visit(Traversal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(HasStringArgumentAndOptionalStringLiteralVarargsRest instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Predicate instance) {
      return otherwise((instance));
    }
    
    default R visit(StringObject instance) {
      return otherwise((instance));
    }
    
    default R visit(StringPredicate instance) {
      return otherwise((instance));
    }
    
    default R visit(Traversal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Object_ extends hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Object_ (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) (other);
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
  
  public static final class Predicate extends hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicate value;
    
    public Predicate (hydra.langs.tinkerpop.gremlin.TraversalPredicate value) {
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
  
  public static final class StringObject extends hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument value;
    
    public StringObject (hydra.langs.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringObject)) {
        return false;
      }
      StringObject o = (StringObject) (other);
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
  
  public static final class StringPredicate extends hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate value;
    
    public StringPredicate (hydra.langs.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringPredicate)) {
        return false;
      }
      StringPredicate o = (StringPredicate) (other);
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
  
  public static final class Traversal extends hydra.langs.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest implements Serializable {
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