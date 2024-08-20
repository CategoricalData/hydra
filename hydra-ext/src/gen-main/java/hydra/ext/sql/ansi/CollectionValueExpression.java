// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public abstract class CollectionValueExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.CollectionValueExpression");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  public static final hydra.core.Name FIELD_NAME_MULTISET = new hydra.core.Name("multiset");
  
  private CollectionValueExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Array instance) ;
    
    R visit(Multiset instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CollectionValueExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
    
    default R visit(Multiset instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Array extends hydra.ext.sql.ansi.CollectionValueExpression implements Serializable {
    public final hydra.ext.sql.ansi.ArrayValueExpression value;
    
    public Array (hydra.ext.sql.ansi.ArrayValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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
  
  public static final class Multiset extends hydra.ext.sql.ansi.CollectionValueExpression implements Serializable {
    public final hydra.ext.sql.ansi.MultisetValueExpression value;
    
    public Multiset (hydra.ext.sql.ansi.MultisetValueExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Multiset)) {
        return false;
      }
      Multiset o = (Multiset) (other);
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