// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SequencePattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SequencePattern");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  private SequencePattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(List instance) ;
    
    R visit(Tuple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SequencePattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class List extends hydra.ext.python.syntax.SequencePattern implements Serializable {
    public final hydra.util.Opt<hydra.ext.python.syntax.MaybeSequencePattern> value;
    
    public List (hydra.util.Opt<hydra.ext.python.syntax.MaybeSequencePattern> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Tuple extends hydra.ext.python.syntax.SequencePattern implements Serializable {
    public final hydra.util.Opt<hydra.ext.python.syntax.OpenSequencePattern> value;
    
    public Tuple (hydra.util.Opt<hydra.ext.python.syntax.OpenSequencePattern> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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