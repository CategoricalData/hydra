// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class DoubleStarredKvpair implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.DoubleStarredKvpair");
  
  public static final hydra.core.Name FIELD_NAME_STARRED = new hydra.core.Name("starred");
  
  public static final hydra.core.Name FIELD_NAME_PAIR = new hydra.core.Name("pair");
  
  private DoubleStarredKvpair () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Starred instance) ;
    
    R visit(Pair instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DoubleStarredKvpair instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Starred instance) {
      return otherwise((instance));
    }
    
    default R visit(Pair instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Starred extends hydra.ext.python.syntax.DoubleStarredKvpair implements Serializable {
    public final hydra.ext.python.syntax.BitwiseOr value;
    
    public Starred (hydra.ext.python.syntax.BitwiseOr value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Starred)) {
        return false;
      }
      Starred o = (Starred) (other);
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
  
  public static final class Pair extends hydra.ext.python.syntax.DoubleStarredKvpair implements Serializable {
    public final hydra.ext.python.syntax.Kvpair value;
    
    public Pair (hydra.ext.python.syntax.Kvpair value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pair)) {
        return false;
      }
      Pair o = (Pair) (other);
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