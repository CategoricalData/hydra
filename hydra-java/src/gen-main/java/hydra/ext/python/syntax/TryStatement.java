// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class TryStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.TryStatement");
  
  public static final hydra.core.Name FIELD_NAME_FINALLY = new hydra.core.Name("finally");
  
  public static final hydra.core.Name FIELD_NAME_EXCEPT = new hydra.core.Name("except");
  
  public static final hydra.core.Name FIELD_NAME_EXCEPT_STAR = new hydra.core.Name("exceptStar");
  
  private TryStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Finally instance) ;
    
    R visit(Except instance) ;
    
    R visit(ExceptStar instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TryStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Finally instance) {
      return otherwise((instance));
    }
    
    default R visit(Except instance) {
      return otherwise((instance));
    }
    
    default R visit(ExceptStar instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Finally extends hydra.ext.python.syntax.TryStatement implements Serializable {
    public final hydra.ext.python.syntax.TryFinallyStatement value;
    
    public Finally (hydra.ext.python.syntax.TryFinallyStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Finally)) {
        return false;
      }
      Finally o = (Finally) (other);
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
  
  public static final class Except extends hydra.ext.python.syntax.TryStatement implements Serializable {
    public final hydra.ext.python.syntax.TryExceptStatement value;
    
    public Except (hydra.ext.python.syntax.TryExceptStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Except)) {
        return false;
      }
      Except o = (Except) (other);
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
  
  public static final class ExceptStar extends hydra.ext.python.syntax.TryStatement implements Serializable {
    public final hydra.ext.python.syntax.TryExceptStarStatement value;
    
    public ExceptStar (hydra.ext.python.syntax.TryExceptStarStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExceptStar)) {
        return false;
      }
      ExceptStar o = (ExceptStar) (other);
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