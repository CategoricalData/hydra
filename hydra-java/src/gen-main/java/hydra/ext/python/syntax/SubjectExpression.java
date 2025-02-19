// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SubjectExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SubjectExpression");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  private SubjectExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Tuple instance) ;
    
    R visit(Simple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SubjectExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Tuple extends hydra.ext.python.syntax.SubjectExpression implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.StarNamedExpression> value;
    
    public Tuple (java.util.List<hydra.ext.python.syntax.StarNamedExpression> value) {
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
  
  public static final class Simple extends hydra.ext.python.syntax.SubjectExpression implements Serializable {
    public final hydra.ext.python.syntax.NamedExpression value;
    
    public Simple (hydra.ext.python.syntax.NamedExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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