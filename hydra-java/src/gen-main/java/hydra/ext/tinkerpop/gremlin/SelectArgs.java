// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class SelectArgs implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.SelectArgs");
  
  public static final hydra.core.Name FIELD_NAME_COLUMN = new hydra.core.Name("column");
  
  public static final hydra.core.Name FIELD_NAME_POP_STRINGS = new hydra.core.Name("popStrings");
  
  public static final hydra.core.Name FIELD_NAME_POP_TRAVERSAL = new hydra.core.Name("popTraversal");
  
  public static final hydra.core.Name FIELD_NAME_STRINGS = new hydra.core.Name("strings");
  
  public static final hydra.core.Name FIELD_NAME_TRAVERSAL = new hydra.core.Name("traversal");
  
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
  
  public static final class Column extends hydra.ext.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.TraversalColumnArgument value;
    
    public Column (hydra.ext.tinkerpop.gremlin.TraversalColumnArgument value) {
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
  
  public static final class PopStrings extends hydra.ext.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.PopStringsArgument value;
    
    public PopStrings (hydra.ext.tinkerpop.gremlin.PopStringsArgument value) {
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
  
  public static final class PopTraversal extends hydra.ext.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal value;
    
    public PopTraversal (hydra.ext.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal value) {
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
  
  public static final class Strings extends hydra.ext.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final java.util.List<hydra.ext.tinkerpop.gremlin.StringArgument> value;
    
    public Strings (java.util.List<hydra.ext.tinkerpop.gremlin.StringArgument> value) {
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
  
  public static final class Traversal extends hydra.ext.tinkerpop.gremlin.SelectArgs implements Serializable {
    public final hydra.ext.tinkerpop.gremlin.NestedTraversal value;
    
    public Traversal (hydra.ext.tinkerpop.gremlin.NestedTraversal value) {
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
