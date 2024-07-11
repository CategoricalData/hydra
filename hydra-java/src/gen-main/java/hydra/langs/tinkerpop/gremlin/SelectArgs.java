// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class SelectArgs implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.SelectArgs");
  
  private SelectArgs () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Column instance) ;
    
    R visit(PopStrings instance) ;
    
    R visit(PopTraversal instance) ;
    
    R visit(Strings instance) ;
    
    R visit(Traversal instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SelectArgs instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Column instance) {
      return otherwise((instance));
    }
    
    default R visit(PopStrings instance) {
      return otherwise((instance));
    }
    
    default R visit(PopTraversal instance) {
      return otherwise((instance));
    }
    
    default R visit(Strings instance) {
      return otherwise((instance));
    }
    
    default R visit(Traversal instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Column extends hydra.langs.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalColumnArgument value;
    
    public Column (hydra.langs.tinkerpop.gremlin.TraversalColumnArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Column)) {
        return false;
      }
      Column o = (Column) (other);
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
  
  public static final class PopStrings extends hydra.langs.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.PopStringsArgument value;
    
    public PopStrings (hydra.langs.tinkerpop.gremlin.PopStringsArgument value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PopStrings)) {
        return false;
      }
      PopStrings o = (PopStrings) (other);
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
  
  public static final class PopTraversal extends hydra.langs.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal value;
    
    public PopTraversal (hydra.langs.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PopTraversal)) {
        return false;
      }
      PopTraversal o = (PopTraversal) (other);
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
  
  public static final class Strings extends hydra.langs.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringArgument> value;
    
    public Strings (java.util.List<hydra.langs.tinkerpop.gremlin.StringArgument> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Strings)) {
        return false;
      }
      Strings o = (Strings) (other);
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
  
  public static final class Traversal extends hydra.langs.tinkerpop.gremlin.SelectArgs implements Serializable {
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